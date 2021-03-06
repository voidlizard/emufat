-- Copyright (c) 2014, Dmitry Zuikov
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

-- * Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.

-- * Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.

-- * Neither the name of emufat nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable, BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}
module FatGenAPI (Entry, EntryInfo(..), CallbackType(..), ClusterTable(..), FAT32GenInfo(..),
                  allocate, filesystem, dir, file, emptyDir,
                  gigs, megs, compileRules, genFileAllocTableRaw,
                  genFileAllocTable, fatSize, calcVolSize, genFATRules,
                  calcDataSize, emptyFile, sectorBlock
                 ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import System.Time
import Text.Printf
import Data.Word (Word8, Word32, Word16, Word64)
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

import Data.Either
import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data
import Data.Binary.Put
import Data.Binary.Get
import System.Random

import FAT
import Encode
import VMCode
import EncodeVM (Block)
import Util

data EntryInfo = EntryInfo { fileId :: Word64, fileName :: String, fileSize :: Word64 }

data CallbackType = CB_STREAM | CB_FILE
  deriving (Eq, Ord, Enum, Data, Typeable, Show)

data Entry =  DirRoot    Word64 [Entry]
            | DirDot     Word64
            | DirDotDot  Word64
            | Dir Word64 String [Entry]
            | File CallbackType Word64 String Word64 BS.ByteString
  deriving (Eq, Ord, Data, Typeable, Show)

entries :: Entry -> [Entry]
entries (DirRoot _ es) = es
entries (Dir _ _ es) = es
entries _ = []

newtype EntryIdT m a = EntryIdT {
    runF :: (WriterT [Entry] (StateT (Word64, Word64) m)) a
} deriving (Applicative, Functor, Monad, MonadWriter [Entry], MonadState (Word64, Word64))

type EntryIdM = EntryIdT Identity

runEntryIdM :: (Word64, Word64) -> EntryIdM a -> ([Entry], (Word64, Word64))
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

file :: String -> CallbackType -> Word64 -> (EntryInfo -> BS.ByteString) -> EntryIdM ()
file nm c sz fn = do
  (n,p) <- get
  tell [File c n nm sz (fn (EntryInfo n nm sz))]
  put (n+1, p)

emptyFile :: (EntryInfo -> BS.ByteString)
emptyFile = const (BS.replicate (fromIntegral fatSectLen) 0)

emptyDir :: EntryIdM ()
emptyDir = tell []

sectorBlock :: Word64 -> BS.ByteString -> Rule
sectorBlock s d = REQ s $ encodeBlock d

isFile :: Entry -> Bool
isFile (File _ _ _ _ _) = True
isFile _ = False

allocTable :: FAT -> Word64 -> Word64 -> BS.ByteString
allocTable fat from len = runPut $ mapM_ putWord32le clusters
  where clusters = [w32 from .. w32 (from + clNum - 2)] ++ [fatLastCluster32]
        clNum = ceiling (fromIntegral ( len `div` bpc ))
        bpc = fatBytesPerCluster fat
        w32 :: Word64 -> Word32
        w32 x = fromIntegral x

megs :: Word64 -> Word64
megs = fromIntegral . ((*) (1024*1024))

gigs :: Word64 -> Word64
gigs = fromIntegral . ((*) 1024) . megs

fatDirLenB :: [Entry] -> Word64
fatDirLenB = sum . map eLen
  where eLen (DirRoot _ _) = 0
        eLen (DirDot _)  = 32
        eLen (DirDotDot _) = 32
        eLen (Dir _ nm es) | length nm < 12 = 32
                         | otherwise = error "Long names are unsupported yet"
        eLen (File _ _ nm _ _) | length nm < 12 = 32
                           | otherwise = error "Long names are unsupported yet"


fatDataEntries :: ClustSize32 -> Entry -> [(Entry, Word64)]
fatDataEntries cl e = [entry x | x <- universe e]
  where entry e  = (e, entryLen cl e)

entryLen :: ClustSize32 -> Entry -> Word64
entryLen cl e@(DirRoot _ es)  = fatSizeToClust cl $ fatDirLenB es
entryLen cl e@(DirDot _)      = 0
entryLen cl e@(DirDotDot _)   = 0
entryLen cl e@(Dir _ n es)    = fatSizeToClust cl $ fatDirLenB es
entryLen cl e@(File _ _ n sz _) = fatSizeToClust cl sz

fatMaxFileLen :: [(Entry, Word64)] -> Word64
fatMaxFileLen es = S.findMax $ S.fromList $ map snd es

type FATWriterM = Writer [Rule] -- FATWriterT []

runFATWriter f = execWriter f

data AllocEntry = AllocEntry { beginSect :: Word64
                             , endSect   :: Word64
                             , entry     :: Entry
                             } deriving (Show)

allocate :: ClustSize32 -> Word64 -> Entry -> [AllocEntry]
allocate cl from = eFix . eAlloc . eOrder . filter eFilt . universe
  where eFilt (File _ _ _ _ _) = True
        eFilt (Dir _ _ _)    = True
        eFilt (DirRoot _ _)  = True
        eFilt _            = False
        eOrder = uncurry (++) . partition (not.isFile)
        eAlloc = reverse . snd . foldl' fentry (from, [])
        fentry (n, xs) e =
          let sectors = entryLen cl e `div` fatSectLen
              begin = n
              end   = max n (begin + sectors - 1)
              n'    = n + sectors
              allocated = AllocEntry begin end e
          in (n', allocated : xs)
        eFix = id

allocateMap :: [AllocEntry] -> M.Map Entry Word64
allocateMap = M.fromList . map (\x -> (entry x, beginSect x))

writeEntry :: Maybe CalendarTime -> Word64 -> Entry -> BS.ByteString

writeEntry _ clust (DirRoot _ es) = error "oops"

writeEntry ct clust (Dir _ nm _) =
  entryRecordShort nm 0 clust ct [DIR]

writeEntry ct clust (DirDot q) =
  entryRecordShort "." 0 clust ct [DIR]

writeEntry ct clust (DirDotDot q) =
  entryRecordShort ".." 0 clust ct [DIR]

writeEntry ct clust (File _ _ nm sz _) =
  entryRecordShort nm sz clust ct []

entryRecordShort :: String -> Word64 -> Word64 -> Maybe CalendarTime -> [ATTR] -> BS.ByteString
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

--badChars :: [Word32]
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
generateData ct cl es = mergeRules $ execWriter $ do
  forM_ es $ \(AllocEntry {beginSect = bsect, endSect = esect, entry = e}) -> do
    case e of
      DirRoot _ es  -> writeEntries bsect esect es
      Dir _ _ es    -> writeEntries bsect esect es
      File c _ _ _ bs -> tell [RANGE bsect esect (encodeBlock (BS.take (fromIntegral fatSectLen) bs) ++ [CALLBACK $ toWord8 c])]
  where

      root@(DirRoot i _) = (entry.fromJust)(find isRoot es)
      rootId = case root of
        (DirRoot i _) -> i
        _             -> error "assertion failed"
      isRoot (AllocEntry{entry = (DirRoot _ _)}) = True
      isRoot _ = False

      toWord8 = fromIntegral . fromEnum

      clMap = M.fromList (map ent es)
      ent (AllocEntry{beginSect=bs, entry=(DirRoot eid _)})  = (eid, clustN bs)
      ent (AllocEntry{beginSect=bs, entry=(Dir eid _ _)})    = (eid, clustN bs)
      ent (AllocEntry{beginSect=bs, entry=(File _ eid _ _ _)}) = (eid, clustN bs)
      ent _ = error "assertion failed"

      clOf (DirRoot eid _) = getCl eid
      clOf (Dir eid _ _) = getCl eid
      clOf (DirDot eid)  = getCl eid
      clOf (DirDotDot eid) | eid == rootId = 0
                           | otherwise = getCl eid
      clOf (File _ eid _ _ _) = getCl eid

      getCl n = fromJust (M.lookup n clMap)

      firstSect = (beginSect . head) es
      clustN s = (s - firstSect) `div` (fatSectPerClust cl) + 2

      writeEntries bsect esect =
        encode bsect esect . BS.concat . map (\e -> writeEntry ct (clOf e) e)
--        encode bsect esect . BS.concat . map (\e -> trace ("ENTRY " ++ show e ++ " " ++ show (clOf e)) $ writeEntry ct (clOf e) e)

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

      bslen' = BS.length
      bslen = fromIntegral . BS.length

type ClusterTable = BS.ByteString

genFAT :: ClustSize32 -> Word64 -> [AllocEntry] -> ClusterTable
genFAT cl size alloc = BS.append allocated free
  where
    allocated = runPut $ do
      mapM_ putWord32le [0x0FFFFFF8, fatLastCluster32]
      forM_ alloc $ \(AllocEntry{beginSect=bs, endSect=es}) -> do
        mapM_ putWord32le (take (fromIntegral (clen bs es - 1)) [2 + 1 + start bs .. ])
        putWord32le fatLastCluster32

    free = BS.replicate ((fromIntegral size) - BS.length allocated) 0
    spc  = fatSectPerClust cl
    clen a b = (b - a + 1) `div` spc
    start bs = fromIntegral (bs `div` spc) :: Word32

encodeFAT :: Word64 -> ClusterTable -> [Rule]
encodeFAT from xs = runEncode (eat xs)
  where eat bs | BS.null bs = sector []
               | otherwise  = let (a, b) = BS.splitAt (fromIntegral (fatSectLen)) bs
                                  bn = fromIntegral (BS.length a) `div` 4
                              in sector (runGet (replicateM bn getWord32le) a) >> eat b

        runEncode f = {-# SCC "runEncode" #-} mergeSeries $ evalState (execWriterT f) from

        sector chunk | null chunk = tell []
                     | otherwise  = {-# SCC "sector" #-} do
          i <- lift get
          lift $ modify succ
          let chunk1 = {-# SCC "chunk" #-} chunk
              scc1 = {-# SCC "scc1" #-} (normalize (encodeSeries chunk1))
          tell [REQ i scc1]

        ns = fatSectLen `div` 4

        mergeSeries rules = {-# SCC "ms" #-} execWriter (eat rules)
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
        normalize xs = {-# SCC "norm" #-} normRLE normSer
          where normSer = {-# SCC "normSer" #-}execWriter (mapM_ withSer xs)
                withSer (SER a b) | (b - a) < 4 = {-# SCC "withSer" #-}genBSeq a b
                withSer x = {-# SCC "withSer" #-} tell [x]
                genBSeq 0 0 = {-# SCC "genBSeq0" #-} tell freeBlock
                genBSeq a b = {-# SCC "genBSeq" #-} tell $ encodeBlock (runPut (mapM_ putWord32le [a .. b]))
                normRLE xs = {-# SCC "normRLE" #-} execWriter (eatRLE xs)
                eatRLE (RLE n x : RLE n' x' : xs) | x == x' = {-# SCC "eatRLE" #-}eatRLE (RLE (n+n') x : xs)
                eatRLE (x:y:xs) = {-# SCC "eatRLE" #-}tell [x] >> eatRLE (y:xs)
                eatRLE x = {-# SCC "eatRLE" #-}tell x

freeBlock = encodeBlock (runPut (putWord32le 0))

encodeSeries :: [Word32] -> [Chunk]
encodeSeries xs = {-# SCC "encodeSeries" #-} (snd . runWriter) (eat series0)
  where series0 = map (\x -> SER x x) xs
        eat (SER a b : SER a' b' : xs) | b+1 == a' = eat (SER a b' : xs)
        eat (x:y:xs) = tell [x] >> eat (y:xs)
        eat x = tell x

adjRules sect = map withRule
  where withRule (REQ n c) = REQ (n+sect) c
        withRule (RANGE a b c) = RANGE (a+sect) (b+sect) c

data FAT32GenInfo = FAT32GenInfo { clusterSize   :: ClustSize32
                                 , volSize       :: Word64
                                 , volID         :: Word32
                                 , volLabel      :: String
                                 , fatSectors    :: Word64
                                 , fatFreeSize   :: Maybe Word64
                                 , reservedSect  :: Maybe Word64
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
  putWord32le freecl            --    FreeCount
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
        rsvd' = fromMaybe 32 (reservedSect info)
        rsvd = fromIntegral rsvd' :: Word16
        fsName = take 8 $ BS.unpack $ runPut $ putNameASCII "FAT32"
        freecl = w32 $ maybe 0xFFFFFFFF (\s -> fatClusters cl s - 1) (fatFreeSize info) -- fsck shows we need -1
        w32 = fromIntegral
        addRsvd bs  = runPut $ do
                       putLazyByteString bs
                       replicateM_ (fromIntegral(fatSectLen) * 6 - (len bs)) (putWord8 0)
                       putLazyByteString bs
                       replicateM_ (fromIntegral ((rsvd' * fatSectLen) - (2*(len bs) + fatSectLen*6 - (len bs)))) (putWord8 0)

          where len x = fromIntegral $ BS.length x

fatSize cl dSize = fatLenToSect $ (fatClNum cl dSize) * 4 + 2*4

calcDataSize :: ClustSize32 -> Entry -> Word64
calcDataSize cl e = size
  where allocated = allocate cl 0 e
        start = (beginSect.head) allocated
        end   = (endSect.last) allocated
        size  = (end - start + 1) * fatSectLen

calcVolSize rsvd cl dSize = rsvd*fatSectLen + 2*(fatSize cl dSize) + dSize

genFileAllocTableRaw :: ClustSize32 -> Word64 -> Entry -> ClusterTable
genFileAllocTableRaw cl dSize sample = genFAT cl fs alloc
  where
   fs = fatSize cl dSize
   alloc = allocate cl 0 sample

genFileAllocTable :: Word64 -> ClusterTable -> [Rule]
genFileAllocTable off t = encodeFAT off t

genFATRules :: FAT32GenInfo -> ClusterTable -> CalendarTime -> Entry -> [Rule]
genFATRules fatInfo fat1 ct sample = rules
  where
    rules = {-# SCC "merge" #-} mergeRules $ concat [encodeRaw fatSectLen 0 fatBin, fat, fat2, gen2]
    fatBin  = fatGenBoot32 fatInfo
    fat  = {-# SCC "f1" #-} encodeFAT fatStart fat1
    fatStart = fromIntegral (BS.length fatBin) `div` fatSectLen
    f2sect = (fsect.last) fat
    fat2 = {-# SCC "f2" #-} encodeFAT (f2sect + 1) fat1
    datasect = (fsect.last) fat2
    gen2 = {-# SCC "gen2" #-} generateData (Just ct) clust (allocate clust (datasect + 1) sample)
    clust = clusterSize fatInfo

compileRules :: [Rule] -> [Block]
compileRules rules = vm
  where
    tree = mkCmpTree rules
    vm = mkVMCode tree
