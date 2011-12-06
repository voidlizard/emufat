module EncodeVM where

import Prelude hiding (EQ)
import Data.Word
import Data.Maybe
import Text.Printf
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS

data Opcode =  DUP | DROP
             | CONST
             | JNZ | JZ | JMP | CALL | RET
             | NOT
             | EQ | NEQ | GT | LE | GQ | LQ | RNG
             | LOADSN
             | SER | NSER
             | RLE1 | RLE2 | RLE3 | RLE4 | RLE5 | RLE6 | RLE7 | RLE8
             | RLE16 | RLE32 | RLE64 | RLE128 | RLE256 | RLE512 | RLEN
             | NOP
             | EXIT
  deriving (Enum, Show)


data CmdArg = W32 Word32 | W16 Word16 | W8 Word8 | ADDR Addr

data Addr = ALabel Label | AOffset Int

data Cmd =  Cmd0 Opcode
          | CmdConst Word32
          | Cmd1 Opcode CmdArg 
          | Cmd2 Opcode CmdArg CmdArg
          | Cmd3 Opcode CmdArg CmdArg CmdArg 
          | CmdJmp Opcode Addr
          | CmdCondJmp Opcode Addr
          | CmdLabel Label
          | RawByte Word8

type Label = Int

type Block = (Label, [Cmd])

labelOf :: Block -> Label
labelOf = fst

instance Show CmdArg where
  show (W32 x)  = printf "%08X" x
  show (W16 x)  = printf "%04X" x
  show (W8 x)   = printf "%02X" x
  show (ADDR a) = (show a)

instance Show Addr where
  show (ALabel l)  = "L" ++ show l
  show (AOffset o) = printf "%04X" o

instance Show Cmd where
  show (Cmd0 code)       = ind 1 $ show code
  show (CmdConst w)      = ind 1 $ printf "CONST %d" w
  show (Cmd1 code a)     = ind 1 $ show code ++ " " ++ show a
  show (Cmd2 code a b)   = ind 1 $ show code ++ " " ++ show a ++ " " ++ show b
  show (Cmd3 code a b c) = ind 1 $ show code ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (CmdJmp code a)   = ind 1 $ show code ++ " " ++ show a
  show (CmdCondJmp code a) = ind 1 $ show code ++ " " ++ show a
  show (RawByte m)       = ind 1 $ "BYTE " ++ printf "%02X" m
  show (CmdLabel lbl)    = "L" ++ show lbl ++ ":"

ind x s = concat (replicate x  "    ")  ++ s

toBinary :: [Cmd] -> BS.ByteString
toBinary cmds = runPut $ do
  undefined

  where norm = undefined



