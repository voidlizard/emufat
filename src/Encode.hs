module Encode where

import Data.Word (Word8, Word32)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Writer
import Control.Monad.State
import Data.Binary.Put
import Data.List
import qualified Data.Map as M

import Text.Printf
import Util

data Rule  = REQ Int [Chunk] | RANGE Int Int [Chunk] deriving Show
data Chunk = SEQ BS.ByteString
           | RLE Int Word8 
           | SER Word32 Word32
           | NSER Word32 Int Word32 -- base offset step
           deriving (Eq, Ord)

instance Show Chunk where
  show (SEQ bs)  = printf "SEQ [%s]" (intercalate " " $ hexDump 128 bs)
  show (RLE a n) = printf "RLE %d %d" a n
  show (SER a b) = printf "SER %d %d" a b
  show (NSER a b c) = printf "NSER %d %d %d" a b c

data CmpTree = GEQ Int CmpTree CmpTree | CODE [Rule]
  deriving (Show)

mkCmpTree :: [Rule] -> CmpTree
mkCmpTree r = mkTree' rulemap
  where rulemap = M.fromList $ map (\x -> (fsect x, x)) r
        avg :: Int -> Int -> Int
        avg a b = a + ((b - a) `div` 2)

        splitGeq n m =
          let (a, b, c) = M.splitLookup n m
          in (a, c `M.union` (maybe M.empty (M.singleton n) b))

        mkTree' xs | M.null xs     = CODE [] 
                   | M.size xs < 3 = CODE (map snd (M.toList xs))
                   | otherwise =
          let ks = map fst $ M.toAscList xs
              n = ks !! (length ks `div` 2)
              (le, geq) = splitGeq n xs
          in GEQ n (mkTree' le) (mkTree' geq)

rsect :: Rule -> Int
rsect (REQ n _) = n
rsect (RANGE _ n _) = n

fsect :: Rule -> Int
fsect (REQ n _) = n
fsect (RANGE n _ _) = n

encodeBlock :: BS.ByteString -> [Chunk]
encodeBlock bs = eat [] [] groups
  where groups  = group (BS.unpack bs)
        eat :: [Chunk] -> [Word8] -> [[Word8]] -> [Chunk]
        eat acc seq (x:xs) | length x == 1 = eat acc (head x:seq) xs
                           | length x >  1 = eat (packRle (RLE (length x) (head x)) seq acc) [] xs
        eat acc [] [] = reverse acc
        eat acc seq [] = reverse (packseq seq : acc)
        packRle r [] acc = r : acc
        packRle r seq acc = r : packseq seq : acc
        packseq seq = SEQ (BS.pack (reverse seq))


encodeRaw :: Int -> Int -> BS.ByteString -> [Rule]
encodeRaw blocklen from bs = mergeRules $ evalState (execWriterT (eat bs)) from
  where eat bs | BS.null bs = return () 
               | otherwise = msplit bs block eat
        block chunk = do
          i <- get
          tell [REQ i (encodeBlock chunk)] >> modify succ
 
        msplit xs f1 f2 = let (a, b) = BS.splitAt fsl xs in f1 a >> f2 b
        fsl = fromIntegral blocklen


decodeBlock :: [Chunk] -> BS.ByteString
decodeBlock cs = runPut $ mapM_ chunk cs
  where chunk (SEQ bs)  = putLazyByteString bs
        chunk (RLE n w) = replicateM_ n (putWord8 w)
        chunk _ = undefined

mergeRules :: [Rule] -> [Rule]
mergeRules r = (snd . runWriter) (eat r)
  where eat (REQ a c : REQ b c' : xs) | a+1 == b && c == c' = eat (RANGE a b c : xs)
        eat (REQ a c : RANGE a' b c' : xs) | a+1  == a' && c == c' = eat (RANGE a b c' : xs)
        eat (RANGE a b c : RANGE a' b' c' : xs) | b+1 == a' && c == c' = eat (RANGE a b' c' : xs)
        eat (RANGE a b c : REQ a' c' : xs) | b+1 == a' && c == c' = eat (RANGE a a' c : xs)
        eat (x:y:xs) = tell [x] >> eat (y:xs)
        eat x = tell x

chunks :: Rule -> [Chunk]
chunks (REQ _ c) = c
chunks (RANGE _ _ c) = c

