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
import Random

import FAT
import Encode
import VMCode 
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

type ClusterTable = [Word32] 

getFAT'' :: [(Int, [Word32])] -> ClusterTable
getFAT'' x = execWriter $ do
  tell [0x0FFFFFF8, fatLastCluster32]
  mapM_ (tell . snd) x

genFAT' :: ClustSize32 -> [AllocEntry] -> [(Int, [Word32])]
genFAT' cl alloc = (snd . runWriter) $ do
  forM_ alloc $ \(AllocEntry{beginSect=bs, endSect=es}) -> do
    tell [ (bs, (take (clen bs es - 1) [2 + 1 + start bs .. ]) ++ [fatLastCluster32]) ]
  where spc  = fatSectPerClust cl
        clen a b = (b - a + 1) `div` spc
        w32 x = fromIntegral x :: Word32
        start bs = fromIntegral (bs `div` spc) :: Word32

encodeFAT :: Int -> ClusterTable -> [Rule]
encodeFAT from xs = runEncode (eat xs)
  where eat [] = sector []
        eat xs = sector (take ns xs) >> eat (drop ns xs)

        runEncode f = mergeSeries $ evalState (execWriterT f) from
        
        sector chunk | null chunk = tell []
                     | otherwise  = do
          i <- lift get
          lift $ modify succ
          tell [REQ i (normalize (encodeSeries chunk))]

        ns = fatSectLen `div` 4

        mergeSeries rules = execWriter (eat rules)
          where 
            mCnd1 n n' a b a' b' = n+1 == n' && (b+1) == a' && (b - a) == (b' - a')
            mCnd2 n n' bs off sp a b =
              (n+1) == n' && sp == (b-a+1) && bs+(fromIntegral (n'-off))*sp == a

            eat (REQ n [SER a b] : REQ n' [SER a' b'] : xs) | mCnd1 n n' a b a' b' =
              eat (RANGE n n' [NSER a n (b - a + 1)] : xs)

            eat (RANGE f t [NSER bs off sp] : REQ n [SER a b] : xs) | mCnd2 t n bs off sp a b =
              eat (RANGE f n [NSER bs off sp] : xs)

            eat (x:y:xs) = tell [x] >> eat (y:xs)
            eat x = tell x

        normalize :: [Chunk] -> [Chunk]
        normalize xs = normRLE normSer
          where normSer = execWriter (mapM_ withSer xs)
                withSer (SER a b) | (b - a) < 4 = genBSeq a b
                withSer x = tell [x]
                genBSeq a b = tell $ encodeBlock (runPut (mapM_ putWord32le [a .. b]))
                normRLE xs = execWriter (eatRLE xs)
                eatRLE (RLE n x : RLE n' x' : xs) | x == x' = eatRLE (RLE (n+n') x : xs)
                eatRLE (x:y:xs) = tell [x] >> eatRLE (y:xs)
                eatRLE x = tell x

encodeSeries :: [Word32] -> [Chunk]
encodeSeries xs = (snd . runWriter) (eat series0)
  where series0 = map (\x -> SER x x) xs
        eat (SER a b : SER a' b' : xs) | b+1 == a' = eat (SER a b' : xs)
        eat (x:y:xs) = tell [x] >> eat (y:xs)
        eat x = tell x

adjRules sect = map withRule
  where withRule (REQ n c) = REQ (n+sect) c
        withRule (RANGE a b c) = RANGE (a+sect) (b+sect) c

data FAT32GenInfo = FAT32GenInfo { clusterSize  :: ClustSize32
                                 , volSize      :: Int
                                 , volID        :: Word32
                                 , volLabel     :: String
                                 , fatSectors   :: Int
                                 , reservedSect :: Maybe Int
                                 } deriving (Show)

fatGenBoot32 :: FAT32GenInfo -> BS.ByteString 
fatGenBoot32 info = addRsvd $ runPut $ do
                                -- BOOT AREA   sect0
  putBytes [0xEB, 0x58, 0x90]   --  0 JmpBoot
  putBytes bsOEMName            --    OEMName
  putWord16le bps               --    BytesPerSec
  putWord8 spc                  --    SectPerClust
  putWord16le rsvd              --    ReservedSecCnt
  putWord8 2                    --    NumFATs
  putWord16le 0                 --    RootEntCnt
  putWord16le 0                 --    TotSec16
  putWord8 0xF8                 --    Media
  putWord16le 0                 --    FAT16Sz
  putWord16le 0x3F              --    SectPerTract
  putWord16le 0xFF              --    NumHeads
  putWord32le 0                 --    HiddSec
  putWord32le sectNum           --    TotSec32
                                -- FAT32 Structure
  putWord32le fsect             --    FATSz32
  putWord16le 0                 --    ExtFlags
  putWord16le 0                 --    FSVer
  putWord32le 2                 --    RootClus
  putWord16le 1                 --    FSInfo
  putWord16le 6                 --    BkBootSec
  putBytes (replicate 12 0)     --    Reserved
  putWord8 0                    --    DrvNum
  putWord8 0                    --    Reserved1
  putWord8 0x29                 --    BootSig
  putWord32le (volID info)      --    VolID
  putNameASCII label            --    VolLab, 11 bytes
  putBytes fsName               -- 82 FileSysType
  putBytes (replicate 420 0)    --    OS Boot code
  putWord16le 0xAA55            --    Boot sect. signature
                                -- FS Info Sector, sect1
  putWord32le 0x41615252        --    LeadSig
  putBytes (replicate 480 0)    --    Reserved
  putWord32le 0x61417272        --    StructSig
  putWord32le 0xFFFFFFFF        --    FreeCount
  putWord32le 0xFFFFFFFF        --    NxtFree
  putBytes (replicate 12 0)     --    Reserved
  putWord32le 0xAA550000        --    TrailSign
 
  where bsOEMName = [0x6d, 0x6b, 0x64, 0x6f, 0x73, 0x66, 0x73, 0x00]
        sectNum = w32 (len `div` fatSectLen)
        len = volSize info
        cl  = clusterSize info
        bps = fromIntegral fatSectLen
        spc = fromIntegral (fatSectPerClust cl)
        fsect = w32 $ fatSectors info
        label = volLabel info
        rsvd' = maybe 32 id (reservedSect info)
        rsvd = fromIntegral rsvd' :: Word16
        fsName = take 8 $ BS.unpack $ runPut $ putNameASCII "FAT32"
        w32 = fromIntegral
        addRsvd bs = BS.append bs (BS.replicate rst 0)
          where len = fromIntegral $ BS.length bs
                rst = fromIntegral $ (rsvd' * fatSectLen) - len


main = do

  let clust = CL_4K

  let sample = fatSample2
  let !alloc = allocate clust 0 sample

  newStdGen >>= setStdGen
  volId <- randomW32 

  ct <- getClockTime >>= toCalendarTime
  let gen = generateData (Just ct) clust alloc
  let !fat1 = getFAT'' $ genFAT' clust alloc

  let fatStart = 2

  let !fat  = encodeFAT fatStart fat1

  let adj = rsect $ last fat

  let fat2 = encodeFAT (adj+1) fat1
  let adj2 = rsect $ last fat
  let gen2 = generateData (Just ct) clust (allocate clust (adj2+1) sample) 

  let fatInfo = FAT32GenInfo clust (gigs 2) volId "TEST" (adj - fatStart + 1) Nothing
  let fatBin  = fatGenBoot32 fatInfo

--  print (BS.length fatBin)
--  error "stop"

  let rules = concat [encodeRaw fatSectLen 0 fatBin, fat, fat2, gen2]
  let tree = mkCmpTree rules
  let vm = mkVMCode tree 

  mapM_ print vm 

--  mapM_ (mapM_ print) (slice 4 rules)


--  mapM_  print (encodeRaw 0 fatBin)
--  mapM_  print fat
--  mapM_  print fat2
--  mapM_  print gen2
--  mapM_  print fatdata

--  BS.hPut stdout fatBin

--  mapM_ print fat
--  mapM_ print fatdata

--  putStrLn ""
--  putStrLn "DONE"

randomW32 :: IO Word32
randomW32 = liftM fromIntegral (randomIO :: IO Int)

hex32 :: Word32 -> String
hex32 x = printf "%08X" (fromIntegral x :: Int) 

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
