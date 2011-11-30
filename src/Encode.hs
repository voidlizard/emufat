module Encode where

import Data.Word (Word8)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Writer
import Data.Binary.Put
import Data.List

data Rule  = REQ Int [Chunk] | RANGE Int Int [Chunk] deriving Show
data Chunk = SEQ BS.ByteString | RLE Int Word8 | BLOCK Int deriving (Eq, Ord, Show)

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

chunks :: Rule -> [Chunk]
chunks (REQ _ c) = c
chunks (RANGE _ _ c) = c

