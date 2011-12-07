module CWriter where

import Prelude hiding (EQ, GT)
import Data.List
import Data.Functor.Identity
import Control.Monad.Writer
import Control.Monad.State
import Text.Printf

import EncodeVM

stackCellType = "uint32_t"


stubs :: String
stubs = 
  envFile $ do
    comment "top of the file"
    put "#include <stdint.h>"
    defines
    types
    endl
    envFunc "runDecode" $ do
      indented $ do
        stmt (printf "DEFSTACK(a, %s, 16)" stackCellType)
        stmt (printf "DEFSTACK(r, %s, 8)" stackCellType)
        endl
        forM_ tmps $ \t -> do
          stmt (printf "%s %s" stackCellType t)
        endl

        put "for(;;++op)"
        braces $ indented $ do
          put "switch(*op)"
          braces $ do
            forM_ codes $ \op -> do
              put (printf "case %s:" (show op))
              indented $ decode op
              endl
        put "default:"
        indented $ exit
      
      exitLabel
      indented $ stmt "return 0"

  where
    envFile :: StubM () -> String
    envFile f = intercalate "\n" (evalState (execWriterT f) 0)

    defines :: StubM ()
    defines = do
      opcodes
      endl
      put "#define DEFSTACK(n, t, l) t n[(l)], *n##top = n"
      put "#define RESET(a)   (a##top) = a"
      put "#define PTOP(a)    (a##top)"
      put "#define TOP(a)     (*(a##top))"
      put "#define POP(a)     (*(a##top)--)"
      put "#define PUSH(a,v) (*(++(a##top)) = v)"

    types = endl >> endl >> comment "type declarations" >> endl >> endl

    envFunc :: String -> StubM () -> StubM ()
    envFunc n f = put (printf "int %s()" n) >> braces f

    opcodes = do
      forM_ codes $ \op -> do
        put (printf "#define %-12s %d" (show op) (fromEnum op))

    braces f = do
      put "{" >> endl >> f >> endl >> put "}" >> endl

    codes :: [Opcode]
    codes = [firstCode .. lastCode]

    decode (DUP)     = stmt (push' a (top' a)) >> next
    decode (DROP)    = pop a >> next
    decode (CONST)   = stmt (push' a decode32) >> next
    decode (CRNG)    = exit
    decode (JNZ)     = exit
    decode (JZ)      = exit
    decode (JGQ)     = exit
    decode (JNE)     = exit
    decode (JMP)     = exit
    decode (CALLT)   = exit
    decode (CALL)    = exit
    decode (RET)     = exit
    decode (NOT)     = exit

    decode (EQ)      = do
      stmt $ tmp0 `assign` (pop' a)
      stmt $ tmp1 `assign` (pop' a)
      stmt (push' a (tmp0 `eq` tmp1))
      next

    decode (NEQ)     = exit
    decode (GT)      = exit
    decode (LE)      = exit
    decode (GQ)      = exit
    decode (LQ)      = exit
    decode (RNG)     = exit
    decode (LOADS2)  = exit
    decode (LOADS3)  = exit
    decode (LOADS4)  = exit
    decode (LOADS5)  = exit
    decode (LOADS6)  = exit
    decode (LOADS7)  = exit
    decode (LOADS8)  = exit
    decode (LOADS9)  = exit
    decode (LOADS10) = exit
    decode (LOADSN)  = exit
    decode (SER)     = exit
    decode (NSER)    = exit
    decode (NSER128) = exit
    decode (RLE1)    = exit
    decode (RLE2)    = exit
    decode (RLE3)    = exit
    decode (RLE4)    = exit
    decode (RLE5)    = exit
    decode (RLE6)    = exit
    decode (RLE7)    = exit
    decode (RLE8)    = exit
    decode (RLE16)   = exit
    decode (RLE32)   = exit
    decode (RLE64)   = exit
    decode (RLE128)  = exit
    decode (RLE256)  = exit
    decode (RLE512)  = exit
    decode (RLEN)    = exit
    decode (NOP)     = skipByte >> next >> endl
    decode (EXIT)    = exit

    tmp0 = "tmp0"
    tmp1 = "tmp1"
    tmp2 = "tmp2"
    tmp3 = "tmp3"

    assign :: String -> String -> String
    assign a b = printf "%s = %s" a b

    eq :: String -> String -> String
    eq a b = printf "(%s == %s)" a b

    a = "a"
    r = "r"

    tmps = [ tmp0, tmp1, tmp2, tmp3 ]

    top' :: String -> String
    top' a = printf "TOP(%s)" a

    pop s  = stmt (pop' s)

    pop' :: String -> String
    pop' s = printf "POP(%s)" s

    push' :: String -> String -> String
    push' s v = printf "PUSH(%s, %s)" s v

    push s v = stmt $ push' s v

    decode32 = printf "DECODE32(op)"

    skipByte = stmt "*op++"

    next = stmt "continue" 

    goto s = stmt ("goto " ++ s)

    exit = goto exitLabelName 

    exitLabelName = "_exit"

    exitLabel :: StubM ()
    exitLabel = put (exitLabelName ++ ":")

    nothing :: StubM ()
    nothing = return ()

    indented :: StubM a -> StubM a
    indented f = do 
      modify succ 
      v <- f
      modify pred
      return v

    comment s = put $ "// " ++ s

    put :: String -> StubM ()
    put s = do
      i <- get
      let idt = concat (replicate (i*4) " ")
      tell [(idt ++ s)]

    put' :: String -> StubM ()
    put' s = tell [s]

    endl = put' "" 

    stmt s = put (s ++ ";")

type StubM a = WriterT [String] (StateT Int Identity) a

