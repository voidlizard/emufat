module EncodeVM where

import Prelude hiding (EQ)
import Data.Word
import Data.Maybe
import qualified Data.Map as M
import Data.Functor.Identity
import Text.Printf
import Data.Binary.Put
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString.Lazy as BS

class OpcodeCL a where
  isRLE  :: a -> Bool
  arity0 :: a -> Bool
  arity1 :: a -> Bool
  arity2 :: a -> Bool
  arity3 :: a -> Bool
  firstCode :: a
  lastCode  :: a

data Opcode =  DUP | DROP
             | CONST | CRNG
             | JNZ | JZ | JGQ | JNE | JMP | CALLT | CALL | RET
             | NOT | EQ | NEQ | GT | LE | GQ | LQ | RNG
             | LOADS2 | LOADS3 | LOADS4 | LOADS5 | LOADS6 | LOADS7
             | LOADS8 | LOADS9 | LOADS10 | LOADSN
             | SER | NSER | NSER128
             | RLE1 | RLE2 | RLE3 | RLE4 | RLE5 | RLE6 | RLE7 | RLE8
             | RLE16 | RLE32 | RLE64 | RLE128 | RLE256 | RLE512 | RLEN
             | NOP
             | EXIT
  deriving (Eq, Ord, Enum, Show)


instance OpcodeCL Opcode where
  isRLE  x = x `elem` [RLE1 .. RLEN]
  arity0 x = x `elem` ([DUP, DROP] ++ [NOT .. RNG] ++ [CALLT, RET, NOP, EXIT] ++ [LOADS2 .. LOADS10])
  arity1 x = x `elem` ([CONST] ++ [JNZ .. JMP] ++ [LOADSN] ++ [RLE1 .. RLEN] ++ [CALL])
  arity2 x = x `elem` ([SER, NSER128, CRNG])
  arity3 x = x `elem` ([NSER])
  firstCode = DUP
  lastCode  = EXIT

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

unArg (W8 a)  = fromIntegral a 
unArg (W16 a) = fromIntegral a
unArg (W32 a) = fromIntegral a
unArg (ADDR (ALabel a)) = a
unArg (ADDR (AOffset a)) = a

ind x s = concat (replicate x  "    ")  ++ s

toBinary :: [(Label, [Cmd])] -> BS.ByteString
toBinary cmds = encodeM (pass3 (pass2 (pass1 cmds)))
  where
    pass3 m  = execWriter $ forM_ cmds $ \(l, cs) -> mapM_ (repl m) cs

    pass2 :: [(Label, Int)] -> M.Map Label Int
    pass2 xs = M.fromList (execWriter (foldM_ blockOff 0 xs))
    pass1    = map (\(l,c) -> (l, fromIntegral $ BS.length $ encodeM c))

    encodeM :: [Cmd] -> BS.ByteString
    encodeM c = runPut (mapM_ encode c)

    encode (Cmd0 x)     = putOpcode x 
    encode (CmdConst x) = putOpcode CONST >> putWord32be x

    encode (Cmd1 LOADSN a) | (unArg a) < 256 = putOpcode LOADSN >> putArg8 a
                           | otherwise = error $ "BAD LOADSN ARG" ++ show a

    encode (Cmd1 x a) | isRLE x   = putOpcode x >> putArg8 a
                      | otherwise = putOpcode x >> putArg32 a
    encode (RawByte b)  = putWord8 b
    encode (CmdLabel _) = return ()
    encode (CmdJmp x a) = putOpcode x >> putArg32 (ADDR a)
    encode (CmdCondJmp x a) = putOpcode x >> putArg32 (ADDR a)

    encode (Cmd2 SER a b) = putOpcode SER >> putArg32 a >> putArg32 b
    encode (Cmd2 NSER128 a b) = putOpcode NSER128 >> putArg32 a >> putArg32 b
    encode (Cmd2 CRNG a b) = putOpcode CRNG >> putArg32 a >> putArg32 b
    encode (Cmd3 NSER a b c) = putOpcode NSER >> putArg32 a >> putArg32 b >> putArg32 c
    encode x = error $ "BAD COMMAND " ++ show x

    putOpcode = putWord8 . op
    putArg8 (W32 a) = putWord8 (fromIntegral a)
    putArg8 (W16 a) = putWord8 (fromIntegral a)
    putArg8 (W8 a) = putWord8 (fromIntegral a)
    putArg8 (ADDR _ ) = error "BAD W8 (ADDRESS)"

    putArg32 (W32 a) = putWord8 (fromIntegral a)
    putArg32 (W16 a) = putWord8 (fromIntegral a)
    putArg32 (W8 a) = putWord8 (fromIntegral a)
    putArg32 (ADDR (ALabel a)) = putWord32be (fromIntegral 0)
    putArg32 (ADDR (AOffset a)) = putWord32be (fromIntegral a)

    op :: Opcode -> Word8
    op = fromIntegral . fromEnum

    blockOff sz (l', c) = tell [(l', sz)] >> return (sz + c)

    repl m x@(CmdJmp a (ALabel n)) = repl' (M.lookup n m) x
    repl m x@(CmdCondJmp a (ALabel n)) = repl' (M.lookup n m) x
    repl m x = tell [x]
    
    repl' (Just n) (CmdJmp op x) = tell [CmdJmp op (AOffset n)]
    repl' (Just n) (CmdCondJmp op x) = tell [CmdCondJmp op (AOffset n)]
    repl' Nothing  x = error $ "BAD LABEL " ++ show x

