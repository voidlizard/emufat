module FAT where

import Data.Word (Word32)

data ClustSize32 = CL_512 | CL_4K | CL_8K | CL_16K | CL_32K
  deriving (Eq, Ord)

data FAT = FAT32 { bytesPerSect :: Int 
                 , sectPerClust :: Int
                 , sectPerFAT   :: Int
                 , rootClust    :: Int
                 , resvdSectCnt :: Int
                 , numFat       :: Int
                 } deriving Show

fatBytesPerCluster :: FAT -> Int
fatBytesPerCluster (FAT32 { bytesPerSect = bps, sectPerClust = spc }) = bps*spc

fatLastCluster32 :: Word32
fatLastCluster32 = 0xFFFFFFFF 

fatClusters :: Integral a => ClustSize32 -> a -> a
fatClusters cl n = ceiling (fromIntegral n / ((fromIntegral . fromEnum) cl))

fatSizeToClust :: Integral a => ClustSize32 -> a -> a
fatSizeToClust cl n = (fromIntegral (fromEnum cl)) * (fromIntegral $ fatClusters cl n)

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

