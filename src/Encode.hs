-- Copyright (c) 2014, Dmitry Zuikov
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

-- * Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.

-- * Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.

-- * Neither the name of emufat nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Encode where

import Data.Word (Word8, Word32, Word64)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Writer
import Control.Monad.State
import Data.Binary.Put
import Data.List
import qualified Data.Map as M

import Text.Printf
import Util

data Rule  = REQ Word64 [Chunk] | RANGE Word64 Word64 [Chunk] deriving Show
data Chunk = SEQ BS.ByteString
           | RLE Word64 Word8 
           | SER Word32 Word32
           | NSER Word32 Word64 Word32 -- base offset step
           | CALLBACK Word8
           deriving (Eq, Ord)

instance Show Chunk where
  show (SEQ bs)  = printf "SEQ [%s]" (intercalate " " $ hexDump 128 bs)
  show (RLE a n) = printf "RLE %d %d" a n
  show (SER a b) = printf "SER %d %d" a b
  show (NSER a b c) = printf "NSER %d %d %d" a b c
  show (CALLBACK n) = printf "CALLBACK %d" n

data CmpTree = GEQ Word64 CmpTree CmpTree | CODE [Rule]
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

rsect :: Rule -> Word64
rsect (REQ n _) = n
rsect (RANGE _ n _) = n

fsect :: Rule -> Word64
fsect (REQ n _) = n
fsect (RANGE n _ _) = n

encodeBlock :: BS.ByteString -> [Chunk]
encodeBlock bs = {-# SCC "encodeBlock" #-}eat [] [] groups
  where groups  = group (BS.unpack bs)
        eat :: [Chunk] -> [Word8] -> [[Word8]] -> [Chunk]
        eat acc seq (x:xs) | length x == 1 = eat acc (head x:seq) xs
                           | length x >  1 = eat (packRle (RLE (fromIntegral (length x)) (head x)) seq acc) [] xs
        eat acc [] [] = reverse acc
        eat acc seq [] = reverse (packseq seq : acc)
        packRle r [] acc = r : acc
        packRle r seq acc = r : packseq seq : acc
        packseq seq = SEQ (BS.pack (reverse seq))


encodeRaw :: Word64 -> Word64 -> BS.ByteString -> [Rule]
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
        chunk (RLE n w) = replicateM_ (fromIntegral n) (putWord8 w)
        chunk _ = undefined

mergeRules :: [Rule] -> [Rule]
mergeRules r = execWriter (eat r)
  where eat (REQ a c : REQ b c' : xs) | a+1 == b && c == c' = eat (RANGE a b c : xs)
        eat (REQ a c : RANGE a' b c' : xs) | a+1  == a' && c == c' = eat (RANGE a b c' : xs)
        eat (RANGE a b c : RANGE a' b' c' : xs) | b+1 == a' && c == c' = eat (RANGE a b' c' : xs)
        eat (RANGE a b c : REQ a' c' : xs) | b+1 == a' && c == c' = eat (RANGE a a' c : xs)
        eat (x:y:xs) = tell [x] >> eat (y:xs)
        eat x = tell x

chunks :: Rule -> [Chunk]
chunks (REQ _ c) = c
chunks (RANGE _ _ c) = c

