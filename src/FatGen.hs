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

import FAT
import ReadFAT
import Encode
import Util

data Entry =  DirRoot    Int [Entry]
            | DirDot     Int
            | DirDotDot  Int
            | Dir Int String [Entry]
            | File Int String Int (Maybe Int)
  deriving (Eq, Ord, Data, Typeable, Show)

entries :: Entry -> [Entry]
entries (DirRoot _ es) = es
entries (Dir _ _ es) = es
entries _ = []

newtype EntryIdT m a = EntryIdT {
    runF :: (WriterT [Entry] (StateT (Int, Int) m)) a 
} deriving (Monad, MonadWriter [Entry], MonadState (Int, Int))

type EntryIdM = EntryIdT Identity

runEntryIdM :: (Int, Int) -> EntryIdM a -> ([Entry], (Int, Int))
runEntryIdM init f = runState (execWriterT (runF f)) init

filesystem :: EntryIdM () -> Entry
filesystem f = DirRoot 0 dirs
  where dirs = fst $ runEntryIdM (1,0) f

dir :: String -> EntryIdM () -> EntryIdM ()
dir s f = do
  st@(n,p) <- get
  let (dirs, (n',_)) = runEntryIdM (n+1, n) f
  let dots = [DirDot n, DirDotDot p]
  tell [Dir n s (dots ++ dirs)]
  put (n', p)

file :: String -> Int -> EntryIdM ()
file nm sz = do
  (n,p) <- get
  tell [File n nm sz Nothing]
  put (n+1, p)

emptyDir :: EntryIdM ()
emptyDir = tell []

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

writeEntry ct clust (Dir _ nm _) =
  entryRecordShort nm 0 clust ct [DIR]

writeEntry ct clust (DirDot q) =
  entryRecordShort "." 0 clust ct [DIR]

writeEntry ct clust (DirDotDot q) =
  entryRecordShort ".." 0 clust ct [DIR]

writeEntry ct clust (File _ nm sz _) =
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

generateData :: Maybe CalendarTime -> ClustSize32 -> [AllocEntry] -> [Rule]
generateData ct cl es = mergeRules $ runFATWriter $ do
  forM_ es $ \(AllocEntry {beginSect = bsect, endSect = esect, entry = e}) ->
    case e of
      DirRoot _ es  -> writeEntries bsect esect es
      Dir _ _ es    -> writeEntries bsect esect es
      File _ _ _ sz -> tell [RANGE bsect esect [RLE fatSectLen 0xFF]]
  where writeEntries bsect esect =
          encode bsect esect . BS.concat . map (\e -> writeEntry ct (clustOf e bsect) e)
        clustOf (DirDot fid) bsect = clustOf'' fid bsect
        clustOf (DirDotDot fid) bsect = clustOf'' fid bsect
        clustOf _ n = clustOf' n
        clustOf' n = n `mod` fatSectPerClust cl
        clustOf'' fid bsect = clustOf' (maybe bsect id (M.lookup fid allocMap))
        encode b e bs | b == e    = tell [REQ b (encodeBlock (rest bs b e (bslen bs)))]
                      | otherwise = encodeSect b e (rest bs b e (bslen bs))

        encodeSect b e bs = eatSect b bs
          where
            eatSect from bs | bslen' bs == fsl = tell [REQ from (encodeBlock bs)]
                            | bslen' bs < fsl  = tell [REQ b (encodeBlock (rest bs b e (bslen bs)))]
                            | otherwise = tell [REQ from (encodeBlock (BS.take fsl bs))]
                                          >> eatSect (from+1) (BS.drop fsl bs)
            fsl = fromIntegral fatSectLen

        rest bs a b l =
          let rs = (b - a + 1) * fatSectLen - l
          in if rs > 0 then BS.append bs (BS.replicate (fromIntegral rs) 0x00) else bs

        allocMap = M.fromList $ catMaybes (map pairOf es)
        pairOf (AllocEntry{beginSect=bs, entry=DirDot n}) = Just (n, bs)
        pairOf (AllocEntry{beginSect=bs, entry=DirDotDot n}) = Just (n, bs)
        pairOf _ = Nothing
        
        bslen' = BS.length
        bslen = fromIntegral . BS.length

--fatSample1 = filesystem $ do
--  dir "A" emptyDir 
--  dir "B" emptyDir 

fatSample2 = filesystem $ do
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

main = do
--  let !q = fatEncode CL_512 (fatSample2)
--  print (fatDirLenB (entries fatSample1))
--  print (fatDirLenB (entries fatSample2))
--  let es = fatDataEntries CL_512 fatSample2
--  print es
--  print (fatMaxFileLen es)

  let clust = CL_32K

  let sample = fatSample2
  print (universe sample)

  let alloc = allocate clust 0 sample 
  mapM_ print alloc

  ct <- getClockTime >>= toCalendarTime
  let !gen = generateData (Just ct) clust alloc
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

