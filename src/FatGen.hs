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

--helloFile = const $ BS8.pack "HELLO WORLD!!"
helloFile = const $ BS8.pack "HELLO WORLD!!"

fatSample2 = filesystem $ do
  file "file0" (16384) helloFile 
  dir "A" $ do
    file "file1" (megs 100) helloFile 
    dir "C" $ do
      file "file3" (megs 100) helloFile 
      file "file4" (megs 100) helloFile 
      file "file5" (megs 100) helloFile 
      dir "E" $ emptyDir 
      
  dir "B" $ do
    file "file2" (megs 50) emptyFile

fatSample3 = filesystem $ do
  dir "A" $ do
    dir "B" $ do
      dir "C" $ emptyDir 
      
  dir "D" $ do emptyDir

fatSample4 = filesystem $ do
  file "file1" (megs 100) emptyFile
  file "file2" (megs 100) emptyFile
  file "file3" (megs 100) emptyFile
  file "file4" (megs 100) emptyFile
  file "file5" (megs 100) emptyFile
  file "file6" (megs 100) emptyFile
  file "file7" (megs 100) emptyFile
  file "file8" (megs 100) emptyFile
  file "file9" (megs 100) emptyFile
  file "file10" (megs 100) emptyFile
  file "file11" (megs 100) emptyFile
  file "file12" (megs 100) emptyFile
  file "file13" (megs 100) emptyFile
  file "file14" (megs 100) emptyFile
  file "file15" (megs 100) emptyFile
  file "file16" (megs 100) emptyFile
  file "file17" (megs 100) emptyFile
  file "file18" (megs 100) emptyFile
  file "file19" (megs 100) emptyFile
  file "file20" (megs 100) emptyFile

fatSample6 = filesystem $ do
  file "file1" (gigs 1) emptyFile
  file "file2" (gigs 1) emptyFile
  file "file3" (gigs 1) emptyFile
  file "file4" (gigs 1) emptyFile
  file "file5" (gigs 1) emptyFile
  file "file6" (gigs 1) emptyFile
  file "file7" (gigs 1) emptyFile
  file "file8" (gigs 1) emptyFile
  file "file9" (gigs 1) emptyFile

fatSample5 = filesystem $ do
  file "file1" (gigs 1) helloFile
  file "file2" (gigs 1) helloFile
  file "file3" (gigs 1) helloFile
  file "file4" (gigs 1) helloFile
  file "file5" (gigs 1) helloFile
  file "file6" (gigs 1) helloFile
  file "file7" (gigs 1) helloFile
  file "file8" (gigs 1) helloFile
  file "file9" (gigs 1) helloFile
  file "file10" (gigs 1) helloFile
  file "file11" (gigs 1) helloFile
  file "file12" (gigs 1) helloFile
  file "file13" (gigs 1) helloFile
  file "file14" (gigs 1) helloFile
  file "file15" (gigs 1) helloFile
  file "file16" (gigs 1) helloFile

fatSample7 = filesystem $ do
  file "00.mp3" (gigs 2) $ const $ runPut $ do
    putWord32be 0xCAFEBABE
    putWord8 192
    putWord8 168
    putWord8 1
    putWord8 1
    putWord32be 3000
    putLazyByteString "/jopakita/pechentreski.mp3"
    putWord8 0
 
  file "01.mp3" (gigs 2) $ const $ runPut $ do
    putWord32be 0xCAFEBABE
    putWord8 192
    putWord8 168
    putWord8 1
    putWord8 1
    putWord32be 3000
    putLazyByteString "lalalalala/qlqlqlqlqql/jopakita/pechentreski.mp3"
    putWord8 0


fatSample8 = filesystem $ do
  file "00.mp3" (gigs 2) $ const $ runPut $ do
    putWord32be 0xCAFEBABE
    putWord8 192
    putWord8 168
    putWord8 1
    putWord8 1
    putWord32be 3000
    putLazyByteString "/jopakita/pechentreski.mp3"
    putWord8 0

  file "00.mp3" (gigs 2) $ const $ runPut $ do
    putWord32be 0xCAFEBABE
    putWord8 192
    putWord8 168
    putWord8 1
    putWord8 1
    putWord32be 3000
    putLazyByteString "/jopakita/pechentreski.mp3"
    putWord8 0

main = do
  let cl = CL_32K
  let rsvd  = 32
  let sample = fatSample8
  let dSize = calcDataSize cl sample 

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
        putStrLn $ printf "DATA SIZE: %d " dSize
        putStrLn $ printf "VOL SIZE: %d " volSize

    _ -> do
      putStrLn "Usage: FatGen bin|asm|stubs|opcodes|rules|stats"

randomW32 :: IO Word32
randomW32 = liftM fromIntegral (randomIO :: IO Int)

hex32 :: Word32 -> String
hex32 x = printf "%08X" (fromIntegral x :: Int) 

fatSampleEmpty = filesystem $ do return ()


