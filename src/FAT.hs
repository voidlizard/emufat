module FAT where

import Data.Word (Word32)

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

