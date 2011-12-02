module Util where

import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Word
import Text.Printf
import Control.Monad.Writer

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

