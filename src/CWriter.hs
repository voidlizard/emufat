module CWriter where

import Prelude hiding (EQ, GT)
import Data.List
import Data.Functor.Identity
import Control.Monad.Writer
import Control.Monad.State
import Text.Printf

import EncodeVM

stackCellType = "uint32_t"
codeType = "uint8_t"
outType  = "char"
bSize = "bsize"

stubs :: String
stubs = 
  envFile $ do
    comment "top of the file"
    put "#include <stdint.h>"
    defines
    put $ "#define DECODE32(op) (((uint32_t)(*((op)+0)<<24))|((uint32_t)(*((op)+1)<<16))|((uint32_t)(*((op)+2)<<8))|((uint32_t)(*((op)+3))))"
    put $ "#define DECODE8(op)  (*(op))"
    put $ "#define RLE(n, b, blen, p0, out)  _rle((n), (b), (blen), (p0), (out))"
    put $ "#define LOADBYTES(f, n, blen, p0, out)  _loadbytes((f), (n), (blen), (p0), (out))"
    put $ "#define GENSEQUENCE(a, b, blen, p0, out) _gensequence((a), (b), (blen), (p0), (out))"
    put $ "#define GENSEQUENCEN(top, a, b, s, blen, p0, out) _gensequence(((a) + ((top)-(b))*(s)), (((a) + ((top)-(b))*(s)) + ((blen)/sizeof(uint32_t)) - 1), (blen), (p0), (out))"
    types

    endl
    put $ "extern void _emufat_decode_debug(uint32_t *a, uint32_t *atop, uint32_t *r, uint32_t *rtop, char *outp, char *out);"
    endl

    put $ "static void _rle(uint32_t n, char b, const int blen, char *begin, char **out)"
    braces $ indented $ do
      stmt $ "int i = 0"
      stmt $ "char *p = *out"
      stmt $ "char *pe = begin + blen"
      put  $ "for(i = 0; i < n; i++) { *p++ = b; if( p >= pe ) p = begin; }"
      stmt $ "*out = p"

    put $ "static void _loadbytes(char *from, uint32_t n, const int blen, char *begin, char **out)"
    braces $ indented $ do
      stmt $ "int i = 0"
      stmt $ "char *p = *out"
      stmt $ "char *pe = begin + blen"
      put  $ "for(i = 0; i < n; i++) { *p++ = *from++; if( p >= pe ) p = begin; }"
      stmt $ "*out = p"

    put $ "static void _gensequence(uint32_t a, uint32_t b, const int blen, char *begin, char **out)"
    braces $ indented $ do
      stmt $ "uint32_t i = 0"
      stmt $ "char *p = *out"
      stmt $ "char *pe = begin + blen"
      put  $ "for(i = a; i <= b; i++ )"
      braces $ indented $ do
        stmt $ " *p++ = (i & 0xFF)"
        stmt $ " if(p >= pe) p = begin"
        stmt $ " *p++ = ((i >> 8) & 0xFF)"
        stmt $ " if(p >= pe) p = begin"
        stmt $ " *p++ = ((i >> 16) & 0xFF)"
        stmt $ " if(p >= pe) p = begin"
        stmt $ " *p++ = ((i >> 24) & 0xFF)"
        stmt $ " if(p >= pe) p = begin"
      stmt $ "*out = p"

    endl
    envFunc "int runDecode" [(stackCellType, "n"), (pt codeType, "code"), ("const int", bSize), ((pt outType, "out"))] $ do
      indented $ do
        stmt (printf "DEFSTACK(a, %s, 16)" stackCellType)
        stmt (printf "DEFSTACK(r, %s, 8)" stackCellType)
        endl
        forM_ tmps $ \t -> do
          stmt (printf "%s %s" stackCellType t)
        endl

        stmt (printf "%s *pout = out" outType)

        stmt (pt codeType ++ op `assign` "code")

        endl

        push a "n"

        put "for(;;)"

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
      put "#define NEXT(x)    continue"
      put "#define JUMP(x, b, o)  ((x) = (b) + (o)); continue"
      put "#define SKIP(x, n)  (x += n)"
      put "#define PC(x, b) ((unsigned int)(x - b))"

    types = endl >> endl >> comment "type declarations" >> endl >> endl

    envFunc :: String -> [(String, String)] -> StubM () -> StubM ()
    envFunc n as f = put (printf "%s(%s)" n args) >> braces f
      where args = intercalate ", " (map (\(a,b) -> printf "%s %s" a b) as)

    op = "op"

    pt s = printf "%s *" s

    pc s = printf "op = code + %s" s
    
    pc'  = printf "PC(op, code)"


    next0 = skip "1" >> next
    next1 = skip "5" >> next

    decode (DUP)     = do 
      stmt ( tmp0 `assign` "TOP(a)")
      push a tmp0
      next0 -- stmt (push' a (top' a)) >> next0

    decode (DROP)    = pop a >> next0
    decode (CONST)   = skip "1" >> stmt (push' a decode32) >> skip "4" >> next

    decode (CRNG)    = do
      skip "1"
      stmt (tmp0 `assign` pop' a)
      stmt (tmp1 `assign` decode32) >> skip "4"
      stmt (tmp2 `assign` decode32) >> skip "4"
      push a ( _and (tmp0 `gq` tmp1) (tmp0 `lq` tmp2) )
      next

    decode (JNZ)     = do
      skip "1"
      stmt (tmp0 `assign` pop' a)
      stmt (tmp1 `assign` decode32) >> skip "4"
      _if1 tmp0 (jump tmp1)
      next

    decode (JZ)      = do 
      skip "1"
      stmt (tmp0 `assign` pop' a)
      stmt (tmp1 `assign` decode32) >> skip "4"
      _if1 (not' tmp0) (jump tmp1)
      next

    decode (JGQ)     = do
      skip "1"
      stmt (tmp1 `assign` pop' a)
      stmt (tmp0 `assign` pop' a)
      stmt (tmp2 `assign` decode32) >> skip "4"
      _if1 (tmp0 `gq` tmp1) (jump tmp2)
      next

    decode (JNE)     = do 
      skip "1"
      stmt (tmp1 `assign` pop' a)
      stmt (tmp0 `assign` pop' a)
      stmt (tmp2 `assign` decode32) >> skip "4"
      _if1 (tmp0 `neq` tmp1) (jump tmp2)
      next

    decode (JMP)     = skip "1" >> jump (decode32)

    decode (CALLT)   = do
      skip "1"
      stmt (tmp0 `assign` pop' a)
      stmt (push' r pc')
      jump tmp0

    decode (CALL)    = do
      skip "1"
      stmt (tmp0 `assign` decode32) >> skip "4"
      stmt (push' r pc')
      jump tmp0

    decode (RET)     = jump (pop' r)

    decode (NOT)     =
      stmt (top' a `assign` (not' (top' a))) >> next0

    decode (EQ)      = binop eq tmp0 tmp1  >> next0
    decode (NEQ)     = binop neq tmp0 tmp1 >> next0
    decode (GT)      = binop gt tmp1 tmp0  >> next0
    decode (LE)      = binop le tmp1 tmp0  >> next0
    decode (GQ)      = binop gq tmp1 tmp0  >> next0
    decode (LQ)      = binop lq tmp1 tmp0  >> next0

    decode (RNG)     = do
      stmt (tmp2 `assign` pop' a)
      stmt (tmp1 `assign` pop' a)
      stmt (tmp0 `assign` pop' a)
      push a ( _and (tmp0 `gq` tmp1) (tmp0 `lq` tmp2) )
      next0

    decode (LOADS2)  = loads 2
    decode (LOADS3)  = loads 3
    decode (LOADS4)  = loads 4
    decode (LOADS5)  = loads 5
    decode (LOADS6)  = loads 6
    decode (LOADS7)  = loads 7
    decode (LOADS8)  = loads 8
    decode (LOADS9)  = loads 9
    decode (LOADS10) = loads 10
    decode (LOADSN)  = loadsn  >> next

    decode (SER)     = do
      skip "1"
      stmt (tmp0 `assign` decode32) >> skip "4"
      stmt (tmp1 `assign` decode32) >> skip "4"
      stmt (printf "GENSEQUENCE(%s, %s, %s, out, &pout)" tmp0 tmp1 bSize)
      next

    decode (NSER)    = do
      skip "1"
      stmt (tmp0 `assign` decode32) >> skip "4"
      stmt (tmp1 `assign` decode32) >> skip "4"
      stmt (tmp2 `assign` decode32) >> skip "4"
      stmt (tmp3 `assign` pop' a)
      stmt (printf "GENSEQUENCEN(%s, %s, %s, %s, %s, out, &pout)" tmp3 tmp0 tmp1 tmp2 bSize)
      next

    decode (NSER128) = do
      skip "1"
      stmt (tmp0 `assign` decode32) >> skip "4"
      stmt (tmp1 `assign` decode32) >> skip "4"
      stmt (tmp3 `assign` pop' a)
      stmt (printf "GENSEQUENCEN(%s, %s, %s, 128, %s, out, &pout)" tmp3 tmp0 tmp1 bSize)
      next

    decode (RLE1)    = rle 1
    decode (RLE2)    = rle 2 
    decode (RLE3)    = rle 3 
    decode (RLE4)    = rle 4
    decode (RLE5)    = rle 5
    decode (RLE6)    = rle 6
    decode (RLE7)    = rle 7
    decode (RLE8)    = rle 8
    decode (RLE16)   = rle 16
    decode (RLE32)   = rle 32
    decode (RLE64)   = rle 64
    decode (RLE128)  = rle 128
    decode (RLE256)  = rle 256
    decode (RLE512)  = rle 512
    decode (RLEN)    = rlen

    decode (NOP)     = next0

    decode (EXIT)    = exit

    decode (DEBUG)   = do
      stmt "_emufat_decode_debug(a, atop, r, rtop, pout, out)"
      exit

    decode (OUTLE) = do 
      stmt (tmp0 `assign` pop' a)
      outbyte ("tmp0  & 0xFF")
      outbyte ("(tmp0 >> 8) & 0xFF")
      outbyte ("(tmp0 >> 16) & 0xFF")
      outbyte ("(tmp0 >> 24) & 0xFF")
      next0

    decode (OUTBE) = do
      stmt (tmp0 `assign` pop' a)
      outbyte ("(tmp0 >> 24) & 0xFF")
      outbyte ("(tmp0 >> 16) & 0xFF")
      outbyte ("(tmp0 >> 8) & 0xFF")
      outbyte ("tmp0  & 0xFF")
      next0

    decode (OUTB)  = do
      stmt (tmp0 `assign` pop' a)
      outbyte ("tmp0 & 0xFF")
      next0

    tmp0 = "tmp0"
    tmp1 = "tmp1"
    tmp2 = "tmp2"
    tmp3 = "tmp3"

    binop fn t0 t1 = do
      stmt $ t1 `assign` (pop' a)
      stmt $ t0 `assign` (pop' a)
      stmt (push' a (t0 `fn` t1))

    assign :: String -> String -> String
    assign a b = printf "%s = %s" a b

    eq :: String -> String -> String
    eq a b = printf "(%s == %s)" a b

    neq :: String -> String -> String
    neq a b = printf "(%s != %s)" a b

    gt :: String -> String -> String
    gt a b = printf "(%s > %s)" a b

    le a b = printf "(%s < %s)" a b
    lq a b = printf "(%s <= %s)" a b

    gq :: String -> String -> String
    gq a b = printf "(%s >= %s)" a b

    a = "a"
    r = "r"

    tmps = [ tmp0, tmp1, tmp2, tmp3 ]

    not' :: String -> String
    not' s = printf "!(%s)" s

    top' :: String -> String
    top' a = printf "TOP(%s)" a

    pop s  = stmt (pop' s)

    pop' :: String -> String
    pop' s = printf "POP(%s)" s

    push' :: String -> String -> String
    push' s v = printf "PUSH(%s, %s)" s v

    push s v = stmt $ push' s v

    decode32 = printf "DECODE32(op)"
    decode8  = printf "DECODE8(op)"

    skip :: String -> StubM ()
    skip s = stmt (printf "SKIP(op, (%s))" s)

    skipByte = stmt "*op++"

    next = stmt "NEXT(op)"

    goto s = stmt ("goto " ++ s)

    _and :: String -> String -> String
    _and a b = printf "(%s && %s)" a b

    _if1 s f = do
      put (printf "if(%s)" s)
      braces $ indented $ f

    jump v = stmt $ printf "JUMP(op, code, %s)" v

    exit = goto exitLabelName 

    l s = s ++ ":"

    exitLabelName = "_exit"

    exitLabel :: StubM ()
    exitLabel = put (l exitLabelName)


    loads :: Int -> StubM ()
    loads n = do
      skip "1"
      stmt (printf "LOADBYTES(op, %d, %s, out, &pout)" n bSize)
      skip (show (n))
      next

    loadsn :: StubM ()
    loadsn  = do
      skip "1"
      stmt ( tmp0 `assign` pop' a ) 
      stmt (printf "LOADBYTES(op, tmp0, %s, out, &pout)" bSize)
      skip "tmp0"
      next

    rle :: Int -> StubM ()
    rle n = do
      skip "1"
      stmt (tmp0 `assign` decode8) >> skip "1"
      stmt (printf "RLE(%d, (unsigned char)%s, %s, out, &pout)" n tmp0 bSize)
      next

    rlen :: StubM ()
    rlen  = do
      skip "1"
      stmt (tmp0 `assign` decode8) >> skip "1"
      stmt (tmp1 `assign` pop' a)
      stmt (printf "RLE(%s, (unsigned char)%s, %s, out, &pout)" tmp1 tmp0 bSize)
      next

    outbyte :: String -> StubM ()
    outbyte s = do
      stmt (printf "*pout++ = (char)(%s)" s)
      stmt (printf "if(pout >= out + %s) pout = out" bSize)
    
    put = line
    put' = line'

opcodes = do
  forM_ codes $ \op -> do
    line (printf "#define %-12s %d" (show op) (fromEnum op))

codes :: [Opcode]
codes = [firstCode .. lastCode]

nothing :: StubM ()
nothing = return ()

indented :: StubM a -> StubM a
indented f = do 
  modify succ 
  v <- f
  modify pred
  return v

comment :: String -> StubM ()
comment s = line $ "// " ++ s

line :: String -> StubM ()
line s = do
  i <- get
  let idt = concat (replicate (i*4) " ")
  tell [(idt ++ s)]

line' :: String -> StubM ()
line' s = tell [s]

endl :: StubM ()
endl = line' "" 

stmt :: String -> StubM ()
stmt s = line (s ++ ";")

braces :: StubM a -> StubM ()
braces f = do
  line "{" >> f >> line "}" >> endl

envFile :: StubM () -> String
envFile f = intercalate "\n" (evalState (execWriterT f) 0)

type StubM a = WriterT [String] (StateT Int Identity) a

