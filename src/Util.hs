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

module Util where

import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Word
import Text.Printf
import Control.Monad.Writer
import Data.Binary.Put

hexDump :: Int -> BS.ByteString -> [String]
hexDump n s = map (intercalate " " . map hex) (eat [] bytes)
  where bytes = BS.unpack s
        hex :: Word8 -> String
        hex = printf "%02X"
        eat :: [[a]] -> [a] -> [[a]]
        eat acc s | length s <= n = acc ++ [s]
                  | otherwise = eat (acc ++ [take n s]) (drop n s)

slice :: Int -> [a] -> [[a]]
slice n xs = execWriter (eat xs)
  where eat s | length s <= n = tell [s] 
              | otherwise = tell [take n s] >> eat (drop n s)

sliceBS :: Int -> BS.ByteString -> [BS.ByteString]
sliceBS n xs = (snd . runWriter) (eat xs)
  where eat s | bl s <= n = tell [s] 
              | otherwise = tell [BS.take (bn n) s] >> eat (BS.drop (bn n) s)
        bl = fromIntegral . BS.length
        bn = fromIntegral

putBytes = mapM_ putWord8

