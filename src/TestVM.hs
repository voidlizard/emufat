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

{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}
--{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable, BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}
module Main where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import System.Environment
import System.Process
import System.IO
import Data.List
import Data.Word
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Binary.Get hiding (label)

import Debug.Trace
import Text.Printf

import VMCode
import EncodeVM
import Util

type Code = [Block]

data Test = T { tname :: String, tcode :: Code, tcheck :: (BS.ByteString -> Bool) }

type TestSuiteM a = Writer [Test] a

data DUMP = HEX | BIN

--makeTest f = trace (show norm) $ norm
makeTest f = norm
  where
--    norm  = trace (show code) $ normalize ml code
    norm  = normalize ml code
    ml    = snd code'
    code  = fst code'
    code' = runGen'' (newLabel >>= label >> f >> exit) 0

testSuite = execWriter

test :: String -> Code -> (BS.ByteString -> Bool) -> TestSuiteM ()
test n code f = tell [T n code f]

printTest :: Test -> IO ()
printTest (T n code _) = print (hexDump 128 (toBinary code))

testDup   = makeTest $ cnst 2 >> dup >> dup >> outle >> outle >> outle
testDrop  = makeTest $ cnst 0xAB >> cnst 0xCC >> drop_ >> outle
testCrng1 = makeTest $ cnst 66 >> crng 55 77 >> outle
testCrng2 = makeTest $ cnst 55 >>  crng 77 88 >> not_ >> outle
testCrng3 = makeTest $ cnst 90 >> crng 77 88 >> outle
testCrng4 = makeTest $ cnst 77 >> crng 77 88 >> outle
testCrng5 = makeTest $ cnst 88 >> crng 77 88 >> outle
testNop   = makeTest $ nop
testNot   = makeTest $ cnst 1 >> not_ >> outle >> not_ >> outle
testConst1 = makeTest $ cnst 0xAABBCCDD >> outle >> cnst 0xDEADBEEF >> outle >> cnst 0xCAFEBABE >> outle
testOutBe  = makeTest $ cnst 0xDEADF00D >> outbe
testOutB   = makeTest $ cnst 0x000000DD >> outb
testEq1    = makeTest $ cnst 1 >> cnst 1 >> eq >> outle
testEq2    = makeTest $ cnst 2 >> cnst 1 >> neq >> outle

testJump1 = makeTest $ do
  l1 <- newLabel
  l3 <- newLabel

  jmp l1
  cnst 0xAABBCCDD
  outle

  label l3
  cnst 0xDEADBEEF
  outle
  exit

  cnst 0xCACAFAFA
  outle

  label l1

  cnst 0xCAFEBABE
  outle
  jmp l3

testJnz1 = makeTest $ do
  l1 <- newLabel
  l2 <- newLabel

  cnst 0xDEADBEEF
  dup
  jnz l1
  outle

  label l1

  cnst 0xCAFEBABE
  outle

  cnst 0xFABACADA
  cnst 0
  jnz l2
  outle
  exit

  label l2
  cnst 0xFFFFFFFF
  outle

testJz1 = makeTest $ do
  l1 <- newLabel
  l2 <- newLabel

  cnst 0
  dup
  jz l1
  outle

  label l1

  cnst 0xCAFEBABE
  outle

  cnst 0xFABACADA
  cnst 0
  jz l2
  outle
  exit

  label l2
  cnst 0xFFFFFFFF
  outle

testJgq = makeTest $ do
  [l1, l2, l3] <- replicateM 3 newLabel

  cnst 46
  cnst 25
  jgq l1

  cnst 0xFFFFFFFF
  outle

  label l1
  cnst 0xC0DE600D -- 1
  outle

  cnst 42
  cnst 42
  jgq l2
  exit

  label l2
  cnst 0xCAFEBABE -- 2
  outle

  cnst 25
  cnst 46
  jgq  l3
  exit

  label l3
  cnst 0xDEADBEEF
  outle

testJne = makeTest $ do
  [l1, l2] <- replicateM 2 newLabel

  cnst 1
  cnst 2
  jne l1
  exit

  label l1
  cnst 0xCAFEBABE -- 1
  outle

  cnst 1
  cnst 1
  jne l2
  cnst 0xCAFEBABE -- 2
  outle
  exit

  label l2
  cnst 0xFFFFFFFF
  outle

testCallRet1 = makeTest $ do
  l1 <- newLabel

  cnst 0xAABBCCDD -- 1
  outle

  call l1
  cnst 0xCAFEBABE -- 3
  outle
  exit

  label l1
  cnst 0xBABAFACA -- 2
  outle
  ret

  cnst 0xFFFFFFFF
  outle

testNSer1 = makeTest $ do
  cnst 0
  nser 0 0 128

testNSer2 = makeTest $ do
  cnst 3
  nser 129 3 128

testNSer128 = makeTest $ do
  cnst 3
  nser128 129 3

testLoadsN = makeTest $ do
  loadsn' 17 >> replicateM_ 17 (byte 0xCC) >> cnst 0xCAFEBABE >> outle

testLoads n a = makeTest $ loads (replicate n a) >> cnst 0xCAFEBABE >> outle

checkLoads n a = assert $ do
  r <- replicateM n getWord8
  cafe <- getWord32le
  return $ r == (replicate n a) && cafe == 0xCAFEBABE

testCb0 = makeTest $ do
  rle 32 0xFF
  calln 0
  exit

testCb1 = makeTest $ do
  rle 32 0xFF
  calln 2
  exit

helloworld = C8.pack "HELLO WORLD"

tests = testSuite $ do

  test "testDrop"  testDrop (assert $ getWord32le >>= return . (== 0xAB))
  test "testCrng1" testCrng1 (assert $ getWord32le >>= return . (== 1))
  test "testCrng2" testCrng2 (assert $ getWord32le >>= return . (/= 0))
  test "testCrng3" testCrng3 (assert $ getWord32le >>= return . (== 0))
  test "testCrng4" testCrng4 (assert $ getWord32le >>= return . (== 1))
  test "testCrng5" testCrng5 (assert $ getWord32le >>= return . (== 1))
  test "testOutBe" testOutBe (assert $ getWord32be >>= return . (== 0xDEADF00D))
  test "testOutB"  testOutB (assert $ getWord8 >>= return . (==0xDD))
  test "testEq1"   testEq1  (assert $ getWord32le >>= return . (== 1))
  test "testEq3"   testEq2  (assert $ getWord32le >>= return . (== 1))
  test "testNeq1" (makeTest $ cnst 1 >> cnst 1 >> neq >> outle) (assert $ getWord32le >>= return . (== 0))
  test "testNeq2" (makeTest $ cnst 42 >> cnst 24 >> neq >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testGt1"  (makeTest $ cnst 42 >> cnst 24 >> gt >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testGt2"  (makeTest $ cnst 24 >> cnst 42 >> gt >> outle) (assert $ getWord32le >>= return . (== 0))
  test "testGt3"  (makeTest $ cnst 24 >> cnst 24 >> gt >> outle) (assert $ getWord32le >>= return . (== 0))
  test "testGq1"  (makeTest $ cnst 42 >> cnst 24 >> geq >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testGq2"  (makeTest $ cnst 24 >> cnst 42 >> geq >> outle) (assert $ getWord32le >>= return . (== 0))
  test "testGq3"  (makeTest $ cnst 24 >> cnst 24 >> geq >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testLe1"  (makeTest $ cnst 42 >> cnst 24 >> le >> outle) (assert $ getWord32le >>= return . (== 0))
  test "testLe2"  (makeTest $ cnst 24 >> cnst 42 >> le >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testLe3"  (makeTest $ cnst 24 >> cnst 24 >> le >> outle) (assert $ getWord32le >>= return . (== 0))
  test "testLq1"  (makeTest $ cnst 42 >> cnst 24 >> lq >> outle) (assert $ getWord32le >>= return . (== 0))
  test "testLq2"  (makeTest $ cnst 24 >> cnst 42 >> lq >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testLq3"  (makeTest $ cnst 24 >> cnst 24 >> lq >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testRng1" (makeTest $ cnst 1  >> cnst 24 >> cnst 42 >> rng >> outle) (assert $ getWord32le >>= return . (== 0))
  test "testRng2" (makeTest $ cnst 29 >> cnst 24 >> cnst 42 >> rng >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testRng3" (makeTest $ cnst 24 >> cnst 24 >> cnst 42 >> rng >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testRng4" (makeTest $ cnst 42 >> cnst 24 >> cnst 42 >> rng >> outle) (assert $ getWord32le >>= return . (== 1))
  test "testRng5" (makeTest $ cnst 90 >> cnst 24 >> cnst 42 >> rng >> outle) (assert $ getWord32le >>= return . (== 0))

  test "testNop"  testNop (\bs -> (BS.take 4 bs) == BS.pack [0,0,0,0] && BS.length bs == 512)


  test "testDup"  testDup (assert $ do
                             a <- getWord32le
                             b <- getWord32le
                             c <- getWord32le
                             return $ a == 2 && b == 2 && c == 2
                          )

  test "testNot"  testNot (assert $ do
                             a <- getWord32le
                             b <- getWord32le
                             return $ a == 0 && b == 1
                          )

  test "testConst1" testConst1 (assert $ do
                                  a <- getWord32le
                                  b <- getWord32le
                                  c <- getWord32le
                                  return $ a == 0xAABBCCDD && b == 0xDEADBEEF && c == 0xCAFEBABE
                               )

  test "testJump1"  testJump1  (assert $ do
                                  a <- getWord32le
                                  b <- getWord32le
                                  return $ a == 0xCAFEBABE && b == 0xDEADBEEF
                               )

  test "testJnz1"  testJnz1 (assert $ do
                               a <- getWord32le
                               b <- getWord32le
                               return $ a == 0xCAFEBABE && b == 0xFABACADA
                            )

  test "testJz1"   testJz1 (assert $ do
                               a <- getWord32le
                               b <- getWord32le
                               return $ a == 0xCAFEBABE && b == 0xFFFFFFFF
                            )

  test "testJgq"   testJgq (assert $ do
                              a <- getWord32le
                              b <- getWord32le
                              return $ a == 0xC0DE600D && b == 0xCAFEBABE
                           )

  test "testJne"   testJne (assert $ do
                              a <- getWord32le
                              b <- getWord32le
                              return $ a == 0xCAFEBABE && b == 0xCAFEBABE
                           )

  test "testCallRet1"  testCallRet1 (assert $ do
                              a <- getWord32le
                              b <- getWord32le
                              c <- getWord32le
                              return $ a == 0xAABBCCDD && b == 0xBABAFACA && c == 0xCAFEBABE
                           )

  test "testSer1"  (makeTest $ ser 0 127) (assert $ do
                               r <- replicateM 128 getWord32le
                               return $ r == [0 .. 127]
                            )

  test "testSer2"  (makeTest $ ser 0 255) (assert $ do
                               r <- replicateM 128 getWord32le
                               return $ r == [128 .. 255]
                            )

  test "testSer3"  (makeTest $ ser 0 0 >> ser 1 1 >> ser 2 2) (assert $ do
                               r <- replicateM  3 getWord32le
                               return $ r == [0 .. 2]
                            )

  test "testNSer1" testNSer1  (assert $ do
                                r <- replicateM 128 getWord32le
                                return $ r == [0 .. 127]
                              )

  test "testNSer2" testNSer2  (assert $ do
                                 r <- replicateM 128 getWord32le
                                 let a = 129 + (3-3)*128
                                 return $ r == take 128 [a, a+1 ..]
                              )
  test "testNSer128" testNSer128  (assert $ do
                                     r <- replicateM 128 getWord32le
                                     let a = 129 + (3-3)*128
                                     return $ r == take 128 [a, a+1 ..]
                                  )

  test "testLoads1" (testLoads 1 0xAA) (checkLoads 1 0xAA)
  test "testLoads2" (testLoads 2 0xBB) (checkLoads 2 0xBB)
  test "testLoads3" (testLoads 3 0xBB) (checkLoads 3 0xBB)
  test "testLoads4" (testLoads 4 0xBB) (checkLoads 4 0xBB)
  test "testLoads5" (testLoads 5 0xBB) (checkLoads 5 0xBB)
  test "testLoads6" (testLoads 6 0xBB) (checkLoads 6 0xBB)
  test "testLoads7" (testLoads 7 0xBB) (checkLoads 7 0xBB)
  test "testLoads8" (testLoads 8 0xBB) (checkLoads 8 0xBB)
  test "testLoads9" (testLoads 9 0xBB) (checkLoads 9 0xBB)
  test "testLoads10" (testLoads 10 0xBB) (checkLoads 10 0xBB)

  test "testLoadsN" (testLoadsN) (checkLoads 17 0xCC)

  test "testRLE1" (makeTest $ rle 1 0xFF) (assert $ getWord8 >>= return . (==0xFF) )
  test "testRLE2" (makeTest $ rle 2 0xFF) (assert $ replicateM 2 (getWord8) >>=  return . (==(replicate 2 0xFF)))
  test "testRLE3" (makeTest $ rle 3 0xFF) (assert $ replicateM 3 (getWord8) >>=  return . (==(replicate 3 0xFF)))
  test "testRLE4" (makeTest $ rle 4 0xFF) (assert $ replicateM 4 (getWord8) >>=  return . (==(replicate 4 0xFF)))
  test "testRLE5" (makeTest $ rle 5 0xFF) (assert $ replicateM 5 (getWord8) >>=  return . (==(replicate 5 0xFF)))
  test "testRLE6" (makeTest $ rle 6 0xFF) (assert $ replicateM 6 (getWord8) >>=  return . (==(replicate 6 0xFF)))
  test "testRLE7" (makeTest $ rle 7 0xFF) (assert $ replicateM 7 (getWord8) >>=  return . (==(replicate 7 0xFF)))
  test "testRLE8" (makeTest $ rle 8 0xFF) (assert $ replicateM 8 (getWord8) >>=  return . (==(replicate 8 0xFF)))
  test "testRLE16" (makeTest $ rle 16 0xFF) (assert $ replicateM 16 (getWord8) >>=  return . (==(replicate 16 0xFF)))
  test "testRLE32" (makeTest $ rle 32 0xFF) (assert $ replicateM 32 (getWord8) >>=  return . (==(replicate 32 0xFF)))
  test "testRLE64" (makeTest $ rle 64 0xFF) (assert $ replicateM 64 (getWord8) >>=  return . (==(replicate 64 0xFF)))
  test "testRLE128" (makeTest $ rle 128 0xFF) (assert $ replicateM 128 (getWord8) >>=  return . (==(replicate 128 0xFF)))
  test "testRLE512" (makeTest $ rle 512 0xFF) (assert $ replicateM 512 (getWord8) >>=  return . (==(replicate 512 0xFF)))
  test "testRLEN"  (makeTest $ cnst 384 >> rlen 0xFF) (assert $ replicateM 384 (getWord8) >>=  return . (==(replicate 384 0xFF)))

  test "testCb0"  testCb0 (assert $ replicateM 32 (getWord8) >>=  return . (==(replicate 32 0xFF)))
  test "testCb1"  testCb1 (assert $ getLazyByteString (BS.length helloworld) >>= return . (== helloworld) )

assert f bs = runGet f bs


main = do
  args <- getArgs

--  print testJump1

  case args of
    ("run"  : path : []) | (not.null) path   -> mapM_ (runTest path) tests
    ("run"  : path : testid : []) | (not.null) path  -> withTest testid (runTest path)
    ("dump" : tid : _)     -> withTest tid (dumpTest BIN)
    ("dump-hex" : tid : _) -> withTest tid (dumpTest HEX)
    ("ls" : _)             -> putStrLn "" >> mapM_ putStrLn (map tname tests)
    _       -> error "Usage: TestVM run vm-binary-path|dump test-id|dump-hex test-id|ls"

  putStrLn ""

runTest :: String -> Test -> IO Bool
runTest path (T{tname=nm, tcode=code, tcheck = tc})= do
  let bin = toBinary code
  (inp,out,err,pid) <- runInteractiveProcess path [] Nothing Nothing
  BS.hPut inp bin
  hClose inp
  res <- BS.hGetContents out
  let r = tc res
  hPutStrLn stderr (printf "test %-24s : %s" nm (if r then "PASSED" else "FAILED !"))
  return r

withTest :: String -> (Test -> IO Bool) -> IO ()
withTest s f =
  case (find ((==s).tname) tests) of
    Nothing -> error $ "*** no test found: " ++ s
    Just t  -> f t >> return ()

dumpTest BIN t = do
  BS.hPut stdout (toBinary (tcode t))
  hFlush stdout
  return True

dumpTest HEX t = do
  mapM_ putStrLn (hexDump 16 (toBinary (tcode t)))
  return True

