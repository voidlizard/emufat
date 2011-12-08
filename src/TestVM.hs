{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}
--{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable, BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}
module Main where

import Control.Monad
import Control.Monad.Writer
import System.Environment
import System.Process
import System.IO
import Data.List
import qualified Data.ByteString.Lazy as BS

import Text.Printf

import VMCode
import EncodeVM
import Util

type Code = [Block]

data Test = T { tname :: String, tcode :: Code, tcheck :: (BS.ByteString -> Bool) }

type TestSuiteM a = Writer [Test] a

makeTest f = normalize (runGen (newLabel >>= label >> f >> exit) 0)

testSuite = execWriter

test :: String -> Code -> (BS.ByteString -> Bool) -> TestSuiteM ()
test n code f = tell [T n code f]

printTest :: Test -> IO ()
printTest (T n code _) = print (hexDump 128 (toBinary code))

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
  test "testNop"  testNop (const False)
  test "testConst1" testConst1 (const False)

main = do
  args <- getArgs
  case args of
    ("run"  : path : []) | (not.null) path   -> mapM_ (runTest path) tests
    ("run"  : path : testid : []) | (not.null) path  -> withTest testid (runTest path)
    ("dump" : tid : _)     -> error "not implemented"
    ("dump-hex" : tid : _) -> error "not implemented"
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

