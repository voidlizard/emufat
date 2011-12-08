{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable, BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}
module Main where

import Control.Monad
import Control.Monad.Writer
import System.Environment

import VMCode
import EncodeVM
import Util

type Code = [Block]

data Test = T { tname :: String, tcode :: Code }

type TestSuiteM a = Writer [Test] a

makeTest f = normalize (runGen (newLabel >>= label >> f >> exit) 0)

testSuite = execWriter

test :: String -> Code -> TestSuiteM ()
test n code = tell [T n code]

printTest :: Test -> IO ()
printTest (T n code) = print (hexDump 128 (toBinary code))

testNop = makeTest $ do
  nop

testConst1 = makeTest $ do
  cnst 0xAABBCCDD
  outle
  cnst 0xDEADBEEF
  outle
  cnst 0xCAFEBABE
  outle
  debug


tests = testSuite $ do
  test "testNop"  testNop
  test "testConst1" testConst1


main = do
  args <- getArgs
  vm <- case args of
          ("run"  : path : _) | (not.null) path   -> return path
          ("dump" : tid : _)     -> error "not implemented"
          ("dump-hex" : tid : _) -> error "not implemented"
          ("ls" : _)             -> error "not implemented"
          _       -> error "Usage: TestVM run vm-binary-path|dump test-id|dump-hex test-id|ls"

  putStrLn ""

--  mapM_ printTest tests

