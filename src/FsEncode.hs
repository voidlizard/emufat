module Main where

import qualified Data.ByteString.Lazy as BS
import System.Environment ( getArgs )
import System.IO (withFile, IOMode(..))
import Text.Printf
import Data.Word (Word8)
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Digest.SHA1 (hash)
import qualified Data.Digest.SHA1 as SHA1
import Util
import Encode


encodeMain :: (String, Int, Int) -> IO ()
encodeMain args@(img, blSz, blNum) = do
  putStrLn $ "encoding" ++ show args
  withFile img ReadMode $ \h -> do
    flip runStateT M.empty $ do
      foldM (blk h) Nothing [0..blNum-1] >>= dump
    return ()
  where blk h Nothing i = do
          bk <- read i h
          return $ Just (REQ i bk)

        blk h v@(Just (REQ i0 c)) i = do
          bk <- read i h
          if c == bk
            then return $ Just $ RANGE i0 i c
            else dump v >> (return $ Just (REQ i bk))

        blk h v@(Just (RANGE i01 i02 c)) i = do
          bk <- read i h
          if c == bk
            then return $ Just $ RANGE i01 i c
            else dump v >> (return $ Just $ REQ i bk)

        dump (Just (REQ  i c)) = liftIO $ putStrLn (printf "EQ %d  " i) >> mapM_ print c >> putStrLn ";"
        dump (Just (RANGE i0 i1 c)) = liftIO $ putStrLn (printf "RANGE %d %d : " i0 i1) >> mapM_ print c >> putStrLn "; "
        dump Nothing  = return ()

        read i h = do
          block <- liftIO $ BS.hGet h blSz
          let encoded = encodeBlock block
          let hsh = block
          exists <- gets (M.member hsh)
          if exists
            then gets (fromJust . M.lookup hsh) >>= \n -> return [BLOCK n]
            else modify (M.insert hsh i) >> return encoded


usageMain = do
  putStrLn "Usage: FsEncode fs_image block_size block_num"

main = do
  args <- getArgs
  case args of
    (img : blSz : blNum : []) -> encodeMain (img, read blSz :: Int, read blNum :: Int)
    _  -> usageMain

