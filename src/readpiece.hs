module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Maybe
import System.Environment
import Text.Printf
--import Control.Exception
import Data.Word

data Chk = W8 | W16 | W32 | W64
data End = BE | LE
data Base = Dec | Hex deriving Eq
data Piece = P8 Word8 | P16 Word16 | P32 Word32 | P64 Word64

data Fmt = Fmt { off :: (Maybe Int), chk :: Chk, end :: End, base ::  Base }

decodeFmt :: String -> String -> String -> String -> Fmt 
decodeFmt x "8" "BE"  b = Fmt (readMaybe x) W8 BE (baseof b)
decodeFmt x "8" "LE"  b = Fmt (readMaybe x) W8 BE (baseof b)
decodeFmt x "16" "BE" b = Fmt (readMaybe x) W16 BE (baseof b)
decodeFmt x "16" "LE" b = Fmt (readMaybe x) W16 LE (baseof b)
decodeFmt x "32" "BE" b = Fmt (readMaybe x) W32 BE (baseof b)
decodeFmt x "32" "LE" b = Fmt (readMaybe x) W32 LE (baseof b)
decodeFmt x "64" "BE" b = Fmt (readMaybe x) W64 BE (baseof b)
decodeFmt x "64" "LE" b = Fmt (readMaybe x) W64 LE (baseof b)
decodeFmt x y z m = error $ "Bad format " ++ (show (x, y, z, m))

printPiece :: Fmt -> Piece -> IO ()
printPiece f m = putStrLn $ pp f m
  where
    pp (Fmt _ _ _ Dec) (P8 x)  = printf "%d" x
    pp (Fmt _ _ _ Dec) (P16 x) = printf "%d" x
    pp (Fmt _ _ _ Dec) (P32 x) = printf "%d" x
    pp (Fmt _ _ _ Dec) (P64 x) = printf "%d" x
    pp (Fmt _ _ _ Hex) (P8 x)  = printf "%02X" x
    pp (Fmt _ _ _ Hex) (P16 x) = printf "%04X" x
    pp (Fmt _ _ _ Hex) (P32 x) = printf "%08X" x
    pp (Fmt _ _ _ Hex) (P64 x) = printf "%016X" x

readPiece :: String -> Fmt -> IO Piece
readPiece fn fmt@(Fmt o _ _ _) = do
  bs <- BS.readFile fn
  return $ flip runGet bs $ do
    maybe (return ()) skip o
    case fmt of
      (Fmt _ W8 _ _)   -> getWord8 >>= return . P8
      (Fmt _ W16 LE _) -> getWord16le >>= return . P16
      (Fmt _ W16 BE _) -> getWord16be >>= return . P16
      (Fmt _ W32 LE _) -> getWord32le >>= return . P32
      (Fmt _ W32 BE _) -> getWord32be >>= return . P32
      (Fmt _ W64 LE _) -> getWord64le >>= return . P64
      (Fmt _ W64 BE _) -> getWord64be >>= return . P64


baseof "H" = Hex
baseof "D" = Dec
baseof _   = Hex

readMaybe :: (Read a) => String -> Maybe a
readMaybe s =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                    [x] -> Just x
                    _   -> Nothing

main = do
  args <- getArgs
  case args of
    ( file : off : x : fmtS : bs : _ ) -> do
      let fmt  = decodeFmt off x fmtS bs
      readPiece file fmt  >>= printPiece fmt

    _ -> error "Usage: readpiece fname off 8|16|32|64 BE|LE H|D"



