module Main where

import qualified Data.ByteString.Lazy as BS
import System.Environment ( getArgs )
import System.IO (withFile, IOMode(..))
import Text.Printf
import Data.Word (Word8)
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Digest.SHA1 (hash)
import qualified Data.Digest.SHA1 as SHA1


data EncBlock = EncBlock [Chunk] deriving Show
data Chunk = SEQ BS.ByteString | RLE Int Word8 | BLOCK Int deriving Show

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
    flip runStateT M.empty $ do
        forM_ [1..blNum] $ \i -> do
          block <- liftIO $ BS.hGet h blSz
          let encoded = encodeBlock block
          let hsh = block -- SHA1.toInteger $ hash (BS.unpack block)
          exists <- gets (M.member hsh)
          if exists
            then gets (fromJust . M.lookup hsh) >>= \n -> out i [BLOCK n]
            else out i encoded >> modify (M.insert hsh i)
          return ()
  putStrLn "DONE"
  where out i chunk = liftIO $ do
        putStrLn (printf "sector %d" i) 
        mapM_ print chunk
        putStrLn ""
--        putStrLn (printf "BLOCK %d" (i-1))
--        putStrLn ""

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

