module Encode where

import Data.Word (Word8, Word32)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Writer
import Data.Binary.Put
import Data.List

data Rule  = REQ Int [Chunk] | RANGE Int Int [Chunk] deriving Show
data Chunk = SEQ BS.ByteString
           | RLE Int Word8 
           | SER Word32 Word32
           | NSER128 Word32 Int Word32 -- base offset step
           | BLOCK Int deriving (Eq, Ord, Show)

rsect :: Rule -> Int
rsect (REQ n _) = n
rsect (RANGE _ n _) = n

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

