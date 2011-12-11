{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable, BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}
module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8 
import System.Environment ( getArgs )
import System.Time
import Text.Printf
import Data.Word (Word8, Word32, Word16)
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor.Identity

import Debug.Trace
import System.IO

import Data.Either
import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data
import Data.Binary.Put
import Data.Binary.Get
import Random

import FAT
import Encode
import VMCode
import EncodeVM (toBinary, Block)
import CWriter
import Util

import FatGenAPI

fatSample2 = filesystem $ do
  file "file0" (16384)
  dir "A" $ do
    file "file1" (megs 100)
    dir "C" $ do
      file "file3" (megs 100)
      file "file4" (megs 100)
      file "file5" (megs 100)
      dir "E" $ emptyDir 
      
  dir "B" $ do
    file "file2" (megs 50)

fatSample3 = filesystem $ do
  dir "A" $ do
    dir "B" $ do
      dir "C" $ emptyDir 
      
  dir "D" $ do emptyDir

fatSample4 = filesystem $ do
  file "file1" (megs 100)
  file "file2" (megs 100)
  file "file3" (megs 100)
  file "file4" (megs 100)
  file "file5" (megs 100)
  file "file6" (megs 100)
  file "file7" (megs 100)
  file "file8" (megs 100)
  file "file9" (megs 100)
  file "file10" (megs 100)
  file "file11" (megs 100)
  file "file12" (megs 100)
  file "file13" (megs 100)
  file "file14" (megs 100)
  file "file15" (megs 100)
  file "file16" (megs 100)
  file "file17" (megs 100)
  file "file18" (megs 100)
  file "file19" (megs 100)
  file "file20" (megs 100)

fatSample5 = filesystem $ do
  file "file1" (gigs 1)
  file "file2" (gigs 1)
  file "file3" (gigs 1)
  file "file4" (gigs 1)
  file "file5" (gigs 1)
  file "file6" (gigs 1)
  file "file7" (gigs 1)
  file "file8" (gigs 1)
  file "file9" (gigs 1)
  file "file10" (gigs 1)
  file "file11" (gigs 1)
  file "file12" (gigs 1)
  file "file13" (gigs 1)
  file "file14" (gigs 1)
  file "file15" (gigs 1)
  file "file16" (gigs 1)
  file "file17" (gigs 1)
  file "file18" (gigs 1)
  file "file19" (gigs 1)
  file "file20" (gigs 1)

main = do
  let cl = CL_4K
  let rsvd  = 32
  let sample = fatSample2
  let dSize = (megs 512)

  newStdGen >>= setStdGen
  volId <- randomW32 
  ct <- getClockTime >>= toCalendarTime

  let fSize = fatSize cl dSize
  let fat1 = genFileAllocTableRaw cl dSize sample 
  let volSize = calcVolSize rsvd cl dSize
  let fatInfo = FAT32GenInfo cl volSize volId "TEST" (fatSectorsOf fSize) (Just rsvd)
  let rules = genFATRules fatInfo fat1 ct sample
  let vm = compileRules rules

  cmd <- getArgs

  case cmd of
    ("asm" : _) -> do
      forM_ vm $ \(l, cmds) -> do
        printf "%-5s\n" ("L" ++ show l ++ ":")
        mapM_ print cmds

    ("bin" : _ ) -> do
      let binary = toBinary vm
      BS.hPut stdout binary

    ("stubs" : _ ) -> do
      putStrLn stubs

    ("opcodes" : _) -> do
      let ops = envFile opcodes
      putStrLn ops

    ("rules" : _) -> do
      mapM_ print rules 

    ("alloc" : _) -> do
      let alloc = allocate cl 0 sample
      mapM_ print alloc

    ("fat" : _) -> do
        BS.hPut stdout fat1
        hFlush stdout

    ("stats" : _) -> do
        putStrLn $ printf "FAT SIZE: %s (%s)" (show fSize) (show (fatSectorsOf fSize))
        putStrLn $ printf "VOL SIZE: %d " volSize

    _ -> do
      putStrLn "Usage: FatGen bin|asm|stubs|opcodes|rules|stats"

randomW32 :: IO Word32
randomW32 = liftM fromIntegral (randomIO :: IO Int)

hex32 :: Word32 -> String
hex32 x = printf "%08X" (fromIntegral x :: Int) 

fatSampleEmpty = filesystem $ do return ()


