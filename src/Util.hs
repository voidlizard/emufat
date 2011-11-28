module Util where

import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Word
import Text.Printf

hexDump :: Int -> BS.ByteString -> [String]
hexDump n s = map (intercalate " " . map hex) (eat [] bytes)
  where bytes = BS.unpack s
        hex :: Word8 -> String
        hex = printf "%02X"
        eat :: [[a]] -> [a] -> [[a]]
        eat acc s | length s <= n = acc ++ [s]
                  | otherwise = eat (acc ++ [take n s]) (drop n s)

