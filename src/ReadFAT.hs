module ReadFAT where

import qualified Data.ByteString.Lazy as BS
import System.IO (withFile, IOMode(..), Handle)
import Control.Monad
import Data.Binary.Get

import FAT

readFAT :: String -> IO FAT
readFAT fname =
  withFile fname ReadMode $ \h -> do
    sect0 <- BS.hGet h 512 
    return $ flip runGet sect0 $ do
        skip $ 0x0B
        bytesPerSect' <- liftM fromIntegral getWord16le
        sectPerClust' <- liftM fromIntegral getWord8
        resvdSectCnt' <- liftM fromIntegral getWord16le -- BPB_RsvdSecCnt
        numFat'       <- liftM fromIntegral getWord8    -- BPB_NumFATs
        skip 0x13 -- skip ...
        sectPerFAT'   <- liftM fromIntegral getWord32le
        skip 4    -- skip
        rootClust'    <- liftM fromIntegral getWord32le
--        skip (0x1FE - (0x2C + 4)) -- signature 
--        sign <- liftM fromIntegral getWord16le
--        trace (printf "%04X" (sign :: Int) :: String) $ return ()
        return $ FAT32 bytesPerSect' sectPerClust' sectPerFAT' rootClust' resvdSectCnt' numFat'

