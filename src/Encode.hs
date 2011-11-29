module Encode where

import Data.Word (Word8)
import Data.ByteString.Lazy as BS

data Rule  = REQ Int [Chunk] | RANGE Int Int [Chunk] deriving Show
data Chunk = SEQ BS.ByteString | RLE Int Word8 | BLOCK Int deriving (Eq, Ord, Show)
