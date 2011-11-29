module Main where

import qualified Data.ByteString.Lazy as BS
import System.Environment ( getArgs )
import Text.Printf
import Data.Word (Word8, Word32)
import Data.List
import Control.Monad
import Data.Binary.Put

import Debug.Trace
import System.IO

import FAT
import ReadFAT
import Util

allocTable :: FAT -> Int -> Int -> BS.ByteString 
allocTable fat from len = runPut $ mapM_ putWord32le clusters
  where clusters = [w32 from .. w32 (from + clNum - 2)] ++ [fatLastCluster32]
        clNum = ceiling (fromIntegral ( len `div` bpc ))
        bpc = fatBytesPerCluster fat
        w32 :: Int -> Word32 
        w32 x = fromIntegral x

main = do
  fn <- liftM (!! 0) getArgs
  fat <- readFAT fn
  let tbl = allocTable fat 3 (100 * 1024 * 1024)
  BS.hPut stdout tbl
--  print (tbl)
--  mapM_ putStrLn (hexDump 32 tbl)

