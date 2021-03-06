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

module FAT where

import Data.Word (Word32, Word8, Word16, Word64)
import Data.Bits
import Data.List
import System.Time

data ClustSize32 = CL_512 | CL_4K | CL_8K | CL_16K | CL_32K
  deriving (Eq, Ord, Show)

data FAT = FAT32 { bytesPerSect :: Word64
                 , sectPerClust :: Word64
                 , sectPerFAT   :: Word64
                 , rootClust    :: Word64
                 , resvdSectCnt :: Word64
                 , numFat       :: Word64
                 } deriving Show

data ATTR = RDONLY | HIDDEN | SYSTEM | VOL_ID | DIR | ARCH

fatBytesPerCluster :: FAT -> Word64
fatBytesPerCluster (FAT32 { bytesPerSect = bps, sectPerClust = spc }) = bps*spc

fatLastCluster32 :: Word32
fatLastCluster32 = 0x0FFFFFFF

fatClusters :: Integral a => ClustSize32 -> a -> a
fatClusters cl n = ceiling (fromIntegral n / ((fromIntegral . fromEnum) cl))

fatSizeToSect = fatClusters

fatSectorsOf :: Word64 -> Word64
fatSectorsOf n = ceiling ((fromIntegral n) / (fromIntegral fatSectLen))

fatSizeToClust :: Integral a => ClustSize32 -> a -> a
fatSizeToClust cl n = (fromIntegral (fromEnum cl)) * (fromIntegral $ fatClusters cl n)

fatLenToSect n = fatSectLen * fatSectorsOf n

fatSectPerClust :: ClustSize32 -> Word64
fatSectPerClust cl = (fromIntegral (fromEnum cl)) `div` (fatSectLen)

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

--fatSectLen :: Word64
fatSectLen = 512

fatSectNum :: ClustSize32 -> Word64
fatSectNum cl = fromIntegral (fromEnum cl) `div` (fromIntegral fatSectLen)

fatClNum :: ClustSize32 -> Word64 -> Word64
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
