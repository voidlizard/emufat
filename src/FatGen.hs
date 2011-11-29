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
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace
import System.IO

import Data.Either
import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data
import Data.Binary.Put

import FAT
import ReadFAT
import Encode
import Util

data Entry =  DirRoot [Entry]
            | DirDot 
            | DirDotDot
            | Dir String [Entry]
            | File String Int (Maybe Int)
  deriving (Eq, Ord, Data, Typeable, Show)

entries :: Entry -> [Entry]
entries (DirRoot es) = es
entries (Dir _ es) = es
entries _ = []

dir :: String -> [Entry] -> Entry
dir nm es = Dir nm $ [DirDot, DirDotDot] ++ es

allocTable :: FAT -> Int -> Int -> BS.ByteString 
allocTable fat from len = runPut $ mapM_ putWord32le clusters
  where clusters = [w32 from .. w32 (from + clNum - 2)] ++ [fatLastCluster32]
        clNum = ceiling (fromIntegral ( len `div` bpc ))
        bpc = fatBytesPerCluster fat
        w32 :: Int -> Word32 
        w32 x = fromIntegral x

megs = ((*) (1024*1024))
gigs = ((*) 1024) . megs

fatSample1 :: Entry
fatSample1 = DirRoot [ File "file1" (megs 100) Nothing ]

fatSample2 :: Entry
fatSample2 = DirRoot [ dir "one" [File "file2" (megs 50) Nothing], File "file1" (megs 100) Nothing]

fatDirLenB :: [Entry] -> Int
fatDirLenB = sum . map eLen
  where eLen (DirRoot _) = 0
        eLen (DirDot)    = 32
        eLen (DirDotDot) = 32
        eLen (Dir nm es) | length nm < 12 = 32
                         | otherwise = error "Long names are unsupported yet"
        eLen (File nm _ _) | length nm < 12 = 32
                           | otherwise = error "Long names are unsupported yet"

fatDataEntries :: ClustSize32 -> Entry -> [(Entry, Int)]
fatDataEntries cl e = [entry x | x <- universe e] 
  where entry e@(DirRoot es)  = (e, clust $ fatDirLenB es)
        entry e@(DirDot)      = (e, 0)
        entry e@(DirDotDot)   = (e, 0)
        entry e@(Dir n es)    = (e, clust $ fatDirLenB es)
        entry e@(File n sz _) = (e, clust sz)
        clust = fatSizeToClust cl

fatMaxFileLen :: [(Entry, Int)] -> Int
fatMaxFileLen es = S.findMax $ S.fromList $ map snd es

data FATWriterState = FATWriterState { nextClust :: Int }

newtype FATWriterT m a = FATWriterM {
    runF :: (StateT FATWriterState (WriterT [Rule] m)) a
} deriving (Monad, MonadWriter [Rule])

type FATWriterM = FATWriterT []

runFATWriter f init = runWriterT (runStateT (runF f) init)

putEntry :: Entry -> FATWriterM () 

putEntry (DirRoot es) = mapM_ putEntry es

putEntry (Dir nm _) = do
  let bytes = entryRecordShort nm 0 0 Nothing [DIR]
  return ()

putEntry (DirDot) = undefined

putEntry (DirDotDot) = undefined

putEntry (File nm sz _) = do
  let bytes = entryRecordShort nm sz 0 Nothing []
  return ()

entryRecordShort :: String -> Int -> Int -> Maybe CalendarTime -> [ATTR] -> BS.ByteString
entryRecordShort nm size clust clk a = runPut $ do
  putNameASCII nm -- Name
  putWord8 (fatAttrB a) -- Attr
  putWord8 0    -- NTRes
  putWord8 0    -- CrtTimeTenth
  putWord16le 0 -- CrtTime
  putWord16le 0 -- CrtDate
  putWord16le 0 -- LstAccDate
  putWord16le cHi -- FstClusHI
  putWord16le 0 -- WrtTime
  putWord16le 0 -- WrdDate
  putWord16le cLo -- FstClusLO
  putWord32le (fromIntegral size) -- FileSize
  where cHi :: Word16
        cHi = fromIntegral $ (fromIntegral clust :: Word32) `shiftR` 16 .&. 0xFFFF
        cLo = (fromIntegral clust :: Word16)

putNameASCII :: String -> Put
putNameASCII s = mapM_ putWord8 bytes
  where bytes :: [Word8]
        bytes = map (fromIntegral . ord) (validFATNameASCII s)

badChars :: [Int]
badChars = [0x22, 0x2A, 0x2C, 0x2F ] ++ [0x3A .. 0x3F] ++ [0x5B .. 0x5D]

validFATNameASCII :: String -> String
validFATNameASCII s = take 11 $ reverse $ compl 11 $ foldl' chr "" s 
  where chr acc c   | ord c < 0x20 || ord c `elem` badChars = '_' : acc
        chr acc _   | length acc == 11 = acc
        chr acc '.' = compl 8 acc
        chr acc c  = c : acc
        compl n s = replicate (n - length s) ' ' ++ s

type WTF = Int

fatEncode :: ClustSize32 -> Entry -> WTF
fatEncode cl r@(DirRoot es) = para item r
  where item (DirRoot es) n = trace "Root entry" $ sum n
        item (DirDot) n = trace "d ." $ sum n
        item (DirDotDot) n = trace "d .." $ sum n
        item (Dir nm es) n = trace ("d " ++ nm) $ sum n
        item (File nm _ _) n = trace ("f " ++ nm) $ sum n

fatEncode _ _ = error "fatEncode: only root dir accepted"

main = do
  putStrLn "PREVED"
  print (fatDirLenB (entries fatSample1))
  print (fatDirLenB (entries fatSample2))
  let es = fatDataEntries CL_32K fatSample2
  print (fatMaxFileLen es)

  let !n = fatEncode CL_512 fatSample2
  putStrLn "DONE"

--  fn <- liftM (!! 0) getArgs
--  fat <- readFAT fn
--  let tbl = allocTable fat 3 (100 * 1024 * 1024)
--  BS.hPut stdout tbl
--  print (tbl)
--  mapM_ putStrLn (hexDump 32 tbl)

