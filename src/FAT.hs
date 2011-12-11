module FAT where

import Data.Word (Word32, Word8, Word16)
import Data.Bits
import Data.List
import System.Time

data ClustSize32 = CL_512 | CL_4K | CL_8K | CL_16K | CL_32K
  deriving (Eq, Ord, Show)

data FAT = FAT32 { bytesPerSect :: Int 
                 , sectPerClust :: Int
                 , sectPerFAT   :: Int
                 , rootClust    :: Int
                 , resvdSectCnt :: Int
                 , numFat       :: Int
                 } deriving Show

data ATTR = RDONLY | HIDDEN | SYSTEM | VOL_ID | DIR | ARCH

fatBytesPerCluster :: FAT -> Int
fatBytesPerCluster (FAT32 { bytesPerSect = bps, sectPerClust = spc }) = bps*spc

fatLastCluster32 :: Word32
fatLastCluster32 = 0x0FFFFFFF

fatClusters :: Integral a => ClustSize32 -> a -> a
fatClusters cl n = ceiling (fromIntegral n / ((fromIntegral . fromEnum) cl))

fatSizeToSect = fatClusters

fatSectorsOf :: Int -> Int
fatSectorsOf n = ceiling ((fromIntegral n) / (fromIntegral fatSectLen))

fatSizeToClust :: Integral a => ClustSize32 -> a -> a
fatSizeToClust cl n = (fromIntegral (fromEnum cl)) * (fromIntegral $ fatClusters cl n)

fatLenToSect n = fatSectLen * fatSectorsOf n

fatSectPerClust cl = (fromEnum cl) `div` (fatSectLen)

fatAttrB :: [ATTR] -> Word8
fatAttrB as = foldl' f  0 as
  where f acc x = acc .|. (fromIntegral . fromEnum) x

fatDate :: CalendarTime -> Word16
fatDate ct = d .|. m .|. y 
  where d = w16 (ctDay ct) .&. 0x1F
        m = (w16 (fromEnum (ctMonth ct) + 1) .&. 0x0F) `shiftL` 5
        y = (w16 (ctYear ct - 1980) .&. 0x7F) `shiftL` 9

fatTime :: CalendarTime -> Word16
fatTime ct = s .|. m .|. h
  where s = w16 (ctSec ct `div` 2) .&. 0x1F
        m = (w16 (ctMin ct) .&. 0x3F) `shiftL` 5
        h = (w16 (ctHour ct) .&. 0x1F) `shiftL` 11

--fatSectLen :: Int
fatSectLen = 512

fatSectNum :: ClustSize32 -> Int 
fatSectNum cl = fromEnum cl `div` (fromIntegral fatSectLen)

fatClNum :: ClustSize32 -> Int -> Int 
fatClNum cl s = ceiling $ (fromIntegral s) / (fromIntegral $ fromEnum cl)

w16 :: Integral a => a -> Word16
w16 = fromIntegral

instance Enum ClustSize32 where
  fromEnum CL_512 =  512
  fromEnum CL_4K  =  4096
  fromEnum CL_8K  =  8192
  fromEnum CL_16K =  16384
  fromEnum CL_32K =  32768
  toEnum  512     = CL_512
  toEnum  4096    = CL_4K
  toEnum  8192    = CL_8K
  toEnum  16384   = CL_16K
  toEnum  32768   = CL_32K

instance Enum ATTR where 
  fromEnum RDONLY = 0x01
  fromEnum HIDDEN = 0x02
  fromEnum SYSTEM = 0x04
  fromEnum VOL_ID = 0x08
  fromEnum DIR    = 0x10
  fromEnum ARCH   = 0x20
  toEnum 0x01 = RDONLY
  toEnum 0x02 = HIDDEN
  toEnum 0x04 = SYSTEM
  toEnum 0x08 = VOL_ID
  toEnum 0x10 = DIR   
  toEnum 0x20 = ARCH



