module Main where

import qualified Data.ByteString.Lazy as BS
import System.Environment ( getArgs )
import System.IO (withFile, IOMode(..))
import Text.Printf
import Data.Word (Word8)
import Data.List
import Control.Monad

data EncBlock = EncBlock [Chunk] deriving Show
data Chunk = SEQ BS.ByteString | RLE Int Word8 deriving Show

hexDump :: Int -> BS.ByteString -> [String]
hexDump n s = map (intercalate " " . map hex) (eat [] bytes)
  where bytes = BS.unpack s
        hex :: Word8 -> String
        hex = printf "%02X"
        eat :: [[a]] -> [a] -> [[a]]
        eat acc s | length s <= n = acc ++ [s]
                  | otherwise = eat (acc ++ [take n s]) (drop n s)

encodeMain :: (String, Int, Int) -> IO ()
encodeMain args@(img, blSz, blNum) = do
  putStrLn $ "encoding" ++ show args
  withFile img ReadMode $ \h -> do
    forM_ [1..blNum] $ \i -> do
      block <- BS.hGet h blSz
      let encoded = encodeBlock block
      putStrLn (printf "BLOCK %d" (i-1))
      mapM_ print encoded
      putStrLn ""
--      mapM_  putStrLn (hexDump 16 block)
--      putStrLn $ "BLOCK READ " ++ show (i - 1)

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

usageMain = do
  putStrLn "Usage: FsEncode fs_image block_size block_num"

main = do
  args <- getArgs
  case args of
    (img : blSz : blNum : []) -> encodeMain (img, read blSz :: Int, read blNum :: Int)
    _  -> usageMain

