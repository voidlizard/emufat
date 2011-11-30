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

data Entry =  DirRoot    Int [Entry]
            | DirDot     (Maybe Int)
            | DirDotDot  (Maybe Int)
            | Dir Int String [Entry]
            | File Int String Int (Maybe Int)
  deriving (Eq, Ord, Data, Typeable, Show)

entries :: Entry -> [Entry]
entries (DirRoot _ es) = es
entries (Dir _ _ es) = es
entries _ = []

dir :: String -> [Entry] -> Entry
dir nm es = Dir 0 nm $ [DirDot Nothing, DirDotDot Nothing] ++ es

isFile :: Entry -> Bool
isFile (File _ _ _ _) = True
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
  where eLen (DirRoot _ _) = 0
        eLen (DirDot _)  = 32
        eLen (DirDotDot _) = 32
        eLen (Dir _ nm es) | length nm < 12 = 32
                         | otherwise = error "Long names are unsupported yet"
        eLen (File _ nm _ _) | length nm < 12 = 32
                           | otherwise = error "Long names are unsupported yet"

fatDataEntries :: ClustSize32 -> Entry -> [(Entry, Int)]
fatDataEntries cl e = [entry x | x <- universe e]
  where entry e  = (e, entryLen cl e)

entryLen :: ClustSize32 -> Entry -> Int        
entryLen cl e@(DirRoot _ es)  = fatSizeToClust cl $ fatDirLenB es
entryLen cl e@(DirDot _)    = 0
entryLen cl e@(DirDotDot _) = 0
entryLen cl e@(Dir _ n es)    = fatSizeToClust cl $ fatDirLenB es
entryLen cl e@(File _ n sz _) = fatSizeToClust cl sz

fatMaxFileLen :: [(Entry, Int)] -> Int
fatMaxFileLen es = S.findMax $ S.fromList $ map snd es

--newtype FATWriterT m a = FATWriterT {
--    runF :: (WriterT [Rule] m) a
--} deriving (Monad, MonadWriter [Rule])

type FATWriterM = Writer [Rule] -- FATWriterT []

runFATWriter f = snd $ runWriter f

data AllocEntry = AllocEntry { beginSect :: Int
                             , endSect   :: Int
                             , entry     :: Entry
                             } deriving (Show)

allocate :: ClustSize32 -> Int -> Entry -> [AllocEntry]
allocate cl from = eFix . eAlloc . eOrder . filter eFilt . universe
  where eFilt (File _ _ _ _) = True
        eFilt (Dir _ _ _)    = True
        eFilt (DirRoot _ _)  = True
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
        eFix = id

allocateMap :: [AllocEntry] -> M.Map Entry Int
allocateMap = M.fromList . map (\x -> (entry x, beginSect x))

writeEntry :: Maybe CalendarTime -> Int -> Entry -> BS.ByteString 

writeEntry _ clust (DirRoot _ es) = error "oops"

writeEntry ct clust (Dir _ nm _) = do
  entryRecordShort nm 0 clust ct [DIR]

writeEntry ct clust (DirDot q) = do
  entryRecordShort "." 0 clust ct [DIR]

writeEntry ct clust (DirDotDot q) = do
  entryRecordShort ".." 0 clust ct [DIR]

writeEntry ct clust (File _ nm sz _) = do
  entryRecordShort nm sz clust ct []

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

validFATNameASCII s | s == "."  = reverse $ compl 11 s
                    | s == ".." = reverse $ compl 11 s

validFATNameASCII s = up $ take 11 $ reverse $ compl 11 $ foldl' chr "" s
  where chr acc c   | ord c < 0x20 || ord c `elem` badChars = '_' : acc
        chr acc _   | length acc == 11 = acc
        chr acc '.' = compl 8 acc
        chr acc c  = c : acc
        up = map toUpper

compl n s = replicate (n - length s) ' ' ++ s

fatSample1 :: Entry
fatSample1 = DirRoot 0 [ File 0 "file1" (megs 100) Nothing ]

fatSample2 :: Entry
fatSample2 = DirRoot 0 [ dir "one" [File 0 "file2" (megs 50) Nothing], File 0 "file1" (megs 100) Nothing]

fatSample3 :: Entry
fatSample3 = DirRoot 0 [ dir "one" [], dir "two" [ dir "four" [] ], dir "three" [ dir "five" [] ]]

generate :: Maybe CalendarTime -> ClustSize32 -> [AllocEntry] -> [Rule]
generate ct cl es = runFATWriter $ do
  forM_ es $ \(AllocEntry {beginSect = bsect, endSect = esect, entry = e}) ->
    case e of
      DirRoot _ es  -> writeEntries bsect esect es
      Dir _ _ es    -> writeEntries bsect esect es
      File _ _ _ sz -> tell [RANGE bsect esect [RLE fatSectLen 0xFF]]
  where writeEntries bsect esect =
          encode bsect esect . BS.concat . map (writeEntry ct (clustOf bsect))
        clustOf n = n `mod` fatSectPerClust cl
        encode b e bs | b == e    = tell [REQ b (encodeBlock bs ++ rest b e (fromIntegral $ BS.length bs))]
                      | otherwise = tell [RANGE b e (encodeBlock bs ++ rest b e (fromIntegral $ BS.length bs))]
        rest a b l = 
          let rs = (b - a + 1) * fatSectLen - l
          in if rs > 0 then [RLE rs 0x00] else []

main = do
--  let !q = fatEncode CL_512 (fatSample2)
--  print (fatDirLenB (entries fatSample1))
--  print (fatDirLenB (entries fatSample2))
--  let es = fatDataEntries CL_512 fatSample2
--  print es
--  print (fatMaxFileLen es)
--  print (universe fatSample2)
--  let !n = fatEncode CL_512 fatSample2
  let alloc = allocate CL_512 0 fatSample3
  mapM_ print alloc
  ct <- getClockTime >>= toCalendarTime
  let !gen = generate (Just ct) CL_512 alloc
  mapM_ print gen
  putStrLn ""
--  let bs = decodeBlock $ concatMap chunks gen
--  BS.hPut stdout bs

--  mapM_ putStrLn (hexDump 16 bs) 

  putStrLn "DONE"

--  fn <- liftM (!! 0) getArgs
--  fat <- readFAT fn
--  let tbl = allocTable fat 3 (100 * 1024 * 1024)
--  BS.hPut stdout tbl
--  print (tbl)
--  mapM_ putStrLn (hexDump 32 tbl)

