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
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get

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
  l2 <- newLabel
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

  label l2
  label l1

  cnst 0xCAFEBABE
  outle
  jmp l3

testJnz1 = makeTest $ do
  l1 <- newLabel
  l2 <- newLabel
  l3 <- newLabel
  l4 <- newLabel

  cnst 0xDEADBEEF
  dup
  jnz l1
  outle

  label l2
  label l1

  cnst 0xCAFEBABE
  outle

  cnst 0xFABACADA
  cnst 0
  jnz l3
  outle
  exit

  label l4
  label l3
  cnst 0xFFFFFFFF
  outle


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
  test "testEq2"   testEq2  (assert $ getWord32le >>= return . (== 1))
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
  hPutStrLn stderr (printf "test %-24s : %s" nm (if r then "PASSED" else "FAILED"))
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

