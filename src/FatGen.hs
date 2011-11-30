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

isFile :: Entry -> Bool
isFile (File _ _ _) = True
isFile _ = False

allocTable :: FAT -> Int -> Int -> BS.ByteString 
allocTable fat from len = runPut $ mapM_ putWord32le clusters
  where clusters = [w32 from .. w32 (from + clNum - 2)] ++ [fatLastCluster32]
        clNum = ceiling (fromIntegral ( len `div` bpc ))
        bpc = fatBytesPerCluster fat
        w32 :: Int -> Word32 
        w32 x = fromIntegral x

megs = ((*) (1024*1024))
gigs = ((*) 1024) . megs

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
  where entry e  = (e, entryLen cl e)

entryLen :: ClustSize32 -> Entry -> Int        
entryLen cl e@(DirRoot es)  = fatSizeToClust cl $ fatDirLenB es
entryLen cl e@(DirDot)      = 0
entryLen cl e@(DirDotDot)   = 0
entryLen cl e@(Dir n es)    = fatSizeToClust cl $ fatDirLenB es
entryLen cl e@(File n sz _) = fatSizeToClust cl sz

fatMaxFileLen :: [(Entry, Int)] -> Int
fatMaxFileLen es = S.findMax $ S.fromList $ map snd es

data FATWriterState = FATWriterState { nextClust :: Int }

data FATWriterOpt = FATWriterOpt { clustSize :: ClustSize32 }

newtype FATWriterT m a = FATWriterM {
    runF :: (ReaderT FATWriterOpt (StateT FATWriterState (WriterT [Rule] m))) a
} deriving (Monad, MonadState FATWriterState, MonadWriter [Rule], MonadReader FATWriterOpt)

type FATWriterM = FATWriterT []

data AllocEntry = AllocEntry { beginSect :: Int
                             , endSect   :: Int
                             , entry     :: Entry
                             } deriving (Show)

allocate :: ClustSize32 -> Int -> Entry -> [AllocEntry]
allocate cl from = eAlloc . eOrder . filter eFilt . universe
  where eFilt (File _ _ _) = True
        eFilt (Dir _ _)    = True
        eFilt (DirRoot _)  = True
        eFilt _            = False
        eOrder = uncurry (++) . partition (not.isFile)
        eAlloc = reverse . snd . foldl' fentry (from, [])
        fentry (n, xs) e =
          let sectors = entryLen cl e `div` fatSectLen
              begin = n
              end   = begin + sectors - 1
              n'    = n + sectors
              allocated = AllocEntry begin end e
          in (n', allocated : xs)

runFATWriter opts init f = runWriterT (runStateT (runReaderT opts (runF f)) init)

putEntry :: Entry -> FATWriterM ()

putEntry (DirRoot es) = error "oops"

putEntry (Dir nm _) = do
  putEncoded $ entryRecordShort nm 0 0 Nothing [DIR]

putEntry (DirDot) = do
  putEncoded $ entryRecordShort "." 0 0 Nothing [DIR]

putEntry (DirDotDot) = do
  putEncoded $ entryRecordShort ".." 0 0 Nothing [DIR]

putEntry (File nm sz _) = do
  putEncoded $ entryRecordShort nm sz 0 Nothing []

putEncoded :: BS.ByteString -> FATWriterM ()
putEncoded bs = do undefined
--  cl    <- liftM (clustSize) ask
--  clust <- gets nextClust
--  let rule = if sectors == 1
--               then REQ   (ss cl clust)
--               else RANGE (ss cl clust) (ss cl clust + sectors)
--  tell $ [rule (encodeBlock bs)]
--  where sectors = fatSizeToSect cl (BS.length bs)
--        ss cl n =  n * fatSectNum cl

entryRecordShort :: String -> Int -> Int -> Maybe CalendarTime -> [ATTR] -> BS.ByteString
entryRecordShort nm size clust clk a = runPut $ do
  putNameASCII nm -- Name
  putWord8 (fatAttrB a) -- Attr
  putWord8 0      -- NTRes
  putWord8 0      -- CrtTimeTenth
  putWord16le cT  -- CrtTime
  putWord16le cD  -- CrtDate
  putWord16le cD  -- LstAccDate
  putWord16le cHi -- FstClusHI
  putWord16le cT  -- WrtTime
  putWord16le cD  -- WrdDate
  putWord16le cLo -- FstClusLO
  putWord32le (fromIntegral size) -- FileSize
  where cHi :: Word16
        cHi = fromIntegral $ (fromIntegral clust :: Word32) `shiftR` 16 .&. 0xFFFF
        cLo = (fromIntegral clust :: Word16)
        cT | isNothing clk =  0
           | otherwise = fatTime (fromJust clk)
        cD | isNothing clk = 0
           | otherwise = fatDate (fromJust clk)

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
fatEncode cl r@(DirRoot es) = 
  trace (show es) $ 0
  where es = universe r
--para item r
--  where item (DirRoot es) n = trace "Put entries: root" $ sum n
--        item (DirDot) n = trace "dot -- skip" $ sum n
--        item (DirDotDot) n = trace "dotdot -- skip" $ sum n
--        item (Dir nm es) n = trace ("dir " ++ nm ++ " put entries") $ sum n
--        item (File nm _ _) n = trace ("file " ++ nm ++ " put content") $ sum n

fatEncode _ _ = error "fatEncode: only root dir allowed"

fatSample1 :: Entry
fatSample1 = DirRoot [ File "file1" (megs 100) Nothing ]

fatSample2 :: Entry
fatSample2 = DirRoot [ dir "one" [File "file2" (megs 50) Nothing], File "file1" (megs 100) Nothing]

main = do
  putStrLn "PREVED"
--  let !q = fatEncode CL_512 (fatSample2)
--  print (fatDirLenB (entries fatSample1))
--  print (fatDirLenB (entries fatSample2))
--  let es = fatDataEntries CL_512 fatSample2
--  print es
--  print (fatMaxFileLen es)
--  print (universe fatSample2)
--  let !n = fatEncode CL_512 fatSample2
  let alloc = allocate CL_512 0 fatSample2
  mapM_ print alloc
  putStrLn "DONE"

--  fn <- liftM (!! 0) getArgs
--  fat <- readFAT fn
--  let tbl = allocTable fat 3 (100 * 1024 * 1024)
--  BS.hPut stdout tbl
--  print (tbl)
--  mapM_ putStrLn (hexDump 32 tbl)

