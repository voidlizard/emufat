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

{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable, BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}
module VMCode (mkVMCode
              ,normalize
              ,runGen, runGen''
              ,newLabel
              ,addr
              ,block
              ,byte
              ,cnst
              ,crng
              ,dup
              ,drop_
              ,eq
              ,exit
              ,outle, outbe, outb
              ,geq, gt, le, lq
              ,jgq
              ,jmp ,jne ,jnz ,jz
              ,call ,ret ,calln
              ,label
              ,ser, nser, nser128
              ,loadsn, loads, loadsn', loads'
              ,neq
              ,op0
              ,op1
              ,op2
              ,op3
              ,rle, rlen
              ,rng
              ,not_
              ,nop ,debug
              ,skip
              ,w16
              ,w32
              ,w8
              ,withLabel
              ,GenT(..)
              ,GenM(..)
              ) where

import Prelude hiding (EQ, GT)
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Identity
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Debug.Trace

import Encode hiding (SER, NSER)
import qualified Encode as E
import EncodeVM
import qualified EncodeVM as V
import Util (slice)

newtype GenT m a = GenT {
    runGT :: (WriterT [Cmd] (StateT (Int) m)) a
} deriving (Applicative, Functor, Monad, MonadWriter [Cmd], MonadState (Int))

type GenM = GenT Identity

runGen :: GenM a -> Int -> [Cmd]
runGen f n = evalState ( execWriterT (runGT f) ) n

runGen'' :: GenM a -> Int -> ([Cmd], Label)
runGen'' f n = runState ( execWriterT (runGT f) ) n

seqs :: [Rule] -> [BS.ByteString]
seqs rs = execWriter $ mapM_ eatR rs
  where eat (SEQ bs) = tell [bs]
        eat _ = tell []
        eatR (REQ _ code) = mapM_ eat code
        eatR (RANGE _ _ code) = mapM_ eat code

newLabel = do
  n <- get
  modify succ >> return n

mkVMCode :: CmpTree -> [Block]
mkVMCode xs = normalize maxl code
  where

    code  = fst code'
    maxl  = snd code'           --  |---- this is shit and hack
    code' = runGen'' (scanT xs >> exit >> subs) (fstBlock) -- FIXME: in fact, it's crap

    fstBlock | M.null seqm = 0
             | otherwise   = (succ.fst) (M.findMax (M.fromList (M.elems seqm)))
    seqm = runEncBS (scanSeq xs)
    scanSeq (GEQ n l r) = scanSeq l >> scanSeq r
    scanSeq (CODE rs) = tell (filter ((>4).BS.length) (seqs rs))

    runEncBS f = bsmap
      where bss   = execWriter f
            bsen  = map (\e -> execWriter (loadsn e)) bss'
            bsen' = zip [0..] bsen
            bsmap = M.fromList (zip bss' bsen')
            bssmap = M.fromList (zip bss (repeat 0))
            counted = execState count bssmap
            count = forM_ bss $ \s -> modify (M.adjust succ s)
            bss' = M.keys $ M.filter (>1) counted

    scanT :: CmpTree -> GenM ()
    scanT (GEQ n left right) = do
      s <- newLabel
      l <- runGen' (scanT left)  >>= withLabel
      r <- runGen' (scanT right) >>= withLabel

      _ex <- newLabel

      label s
      dup
      cnst n
      jgq (labelOf r)
      block l >> jmp _ex
      block r >> label _ex

    scanT (CODE [])    = op0 EXIT
    scanT (CODE rules) = mapM_ scanR rules

    scanR :: Rule -> GenM ()
    scanR ( REQ n code ) = do
      s <- newLabel
      code' <- runGen' (scanmC code) >>= withLabel
      ex <- newLabel

      label s
      dup
      cnst n
      jne ex
      block code'
      label ex

    scanR ( RANGE a b code ) = do
      s <- newLabel
      code' <- runGen' (scanmC code) >>= withLabel
      ex <- newLabel

      label s
      dup
      crng a b
      jz ex
      block code'
      label ex

    scanC :: Chunk -> GenM ()

    scanC ( SEQ bs ) | BS.length bs == 1  =
      rle 1 (head (BS.unpack bs))

    scanC ( SEQ bs )  = do
      case (M.lookup bs seqm) of
        Just (l, seq) -> op1 CALL (ADDR (ALabel l))
        Nothing       -> loadsn bs

    scanC ( E.SER x y ) = ser x y

    scanC ( E.NSER b o st ) | st == 128 = do
      op2 NSER128 (w32 b) (w32 o)

    scanC ( E.NSER b o st ) = do
      op3 NSER (w32 b) (w32 o) (w32 st)

    scanC ( RLE 0 w )   = skip
    scanC ( RLE n w)    = rle n (fromIntegral w)

    scanC ( CALLBACK n ) = calln n

    scanmC xs = mapM_ scanC xs >> op0 EXIT

    subs = mapM_ (\e -> block e >> op0 RET) (M.elems seqm)

    runGen' f = do
      st <- get
      let (r,s) = runState (execWriterT (runGT f)) st
      put s
      return r

skip :: GenM ()
skip   = return ()

dup  = op0 DUP
drop_ = op0 DROP
nop = op0 NOP
not_ = op0 NOT

debug = op0 DEBUG

eq :: GenM ()
eq  = op0 V.EQ
neq = op0 NEQ
geq = op0 GQ
gt  = op0 GT
le  = op0 LE
lq  = op0 LQ

rng = op0 RNG

crng a b = op2 CRNG (w32 a) (w32 b)

outle = op0 OUTLE
outbe = op0 OUTBE
outb  = op0 OUTB

ser x y = op2 SER (w32 x) (w32 y)

nser a b c  = op3 NSER (w32 a) (w32 b) (w32 c)
nser128 a b = op2 NSER128 (w32 a) (w32 b)

jne n = tell [CmdCondJmp JNE (addr n)]
jgq n = tell [CmdCondJmp JGQ (addr n)]
jnz n = tell [CmdCondJmp JNZ (addr n)]
jz  n = tell [CmdCondJmp JZ (addr n)]
jmp n = tell [CmdJmp JMP (addr n)]

calln n = op1 CALLN (w8 n)

call n = tell [Cmd1 CALL (addr' n)]
ret = op0 RET

label n = tell [CmdLabel n]

block (l, ops) = tell [CmdLabel l] >> tell ops

rle 0 x = skip
rle 1 x = op1 RLE1 (w8 x)
rle 2 x = op1 RLE2 (w8 x)
rle 3 x = op1 RLE3 (w8 x)
rle 4 x = op1 RLE4 (w8 x)
rle 5 x = op1 RLE5 (w8 x)
rle 6 x = op1 RLE6 (w8 x)
rle 7 x = op1 RLE7 (w8 x)
rle 8 x = op1 RLE8 (w8 x)
rle 16 x = op1 RLE16 (w8 x)
rle 32 x = op1 RLE32 (w8 x)
rle 64 x = op1 RLE64 (w8 x)
rle 128 x = op1 RLE128 (w8 x)
rle 256 x = op1 RLE256 (w8 x)
rle 512 x = op1 RLE512 (w8 x)
rle n x =  cnst n >> op1 RLEN (w8 x)

rlen x = op1 RLEN (w8 x)

loads' :: Int -> GenM ()
loads' 1  = error "BAD OPCODE: loads1"
loads' 2  = op0 LOADS2
loads' 3  = op0 LOADS3
loads' 4  = op0 LOADS4
loads' 5  = op0 LOADS5
loads' 6  = op0 LOADS6
loads' 7  = op0 LOADS7
loads' 8  = op0 LOADS8
loads' 9  = op0 LOADS9
loads' 10 = op0 LOADS10

loadsn' x = op1 LOADSN (w8 x)

loads :: [Word8] ->  GenM ()
loads x = loadsn (BS.pack x)

loadsn bs | BS.length bs == 1 = op1 RLE1 (w8 (BS.head bs))

loadsn bs = do
  forM_ (slice 256 (BS.unpack bs)) $ \xs -> do
    case bl of
      2 -> op0 LOADS2
      3 -> op0 LOADS3
      4 -> op0 LOADS4
      5 -> op0 LOADS5
      6 -> op0 LOADS6
      7 -> op0 LOADS7
      8 -> op0 LOADS8
      9 -> op0 LOADS9
      10 -> op0 LOADS10
      _ -> op1 LOADSN bl8
    mapM_ byte xs
    where bl = (BS.length bs)
          bl8 = w8 bl
          op0 op = tell [Cmd0 op]


byte x = tell [RawByte (fromIntegral x)]

cnst :: Integral a => a -> GenM ()
cnst x = tell [CmdConst (fromIntegral x)]

op0 :: Opcode -> GenM ()
op0 x = tell [Cmd0 x]

op1 x a = tell [Cmd1 x a]
op2 x a b = tell [Cmd2 x a b]
op3 x a b c = tell [Cmd3 x a b c]

addr :: Label -> Addr
addr l = ALabel l

addr' :: Label -> CmdArg
addr' l = ADDR (ALabel l)

w8 :: Integral a => a -> CmdArg
w8 x = W8 (fromIntegral x :: Word8)

w16 :: Integral a => a -> CmdArg
w16 x = W16 (fromIntegral x :: Word16)

w32 :: Integral a => a -> CmdArg
w32 x = W32 (fromIntegral x :: Word32)

withLabel x = do
  l <- newLabel
  return (l,x)

exit = op0 EXIT

normalize :: Label -> [Cmd] -> [(Label, [Cmd])]
normalize ml xs = map optBlock (mergeBlocks (optJumps blocks))
--normalize xs = trace (intercalate "\n" (map show xs)) $ map optBlock (mergeBlocks (optJumps blocks))
--normalize xs = blocks
  where
        blocks = flip evalState [(ml+1)..] $ execWriterT (eat1 (Nothing, []) xs)

        eat1 (Nothing, []) (CmdLabel n:xs)  = eat1 (Just n, []) xs

        eat1 (Just n, bs)  (CmdLabel n':xs) =
          block (n, normBlock ((CmdJmp JMP (ALabel n')) : bs)) >> eat1 (Just n', []) xs

        eat1 (Just n, bs)  (a@(CmdJmp _ _):xs)  =
          block (n, normBlock (a:bs)) >> eat1 (Nothing, []) xs

        eat1 (Just n, bs)  (a@(CmdCondJmp _ _):CmdLabel n':xs) =
          block (n, normBlock ((CmdJmp JMP (ALabel n')) : a : bs)) >> eat1 (Just n', []) xs

        eat1 (Just n, bs)  (a@(Cmd0 RET):xs)  =
          block (n, normBlock (a:bs)) >> eat1 (Nothing, []) xs

        eat1 (Just n, bs)  (x:xs) = eat1 (Just n, x:bs) xs

        eat1 (Just n, bs) [] = block (n, normBlock bs)

        eat1 (Nothing, []) [] = return ()

        eat1 (Nothing, []) xs = do
          l <- gets head
          modify tail
          eat1 (Just l, []) xs
--          eat1    error "BLOCK STARTED WITH NON-LABEL INS"

        eat1 (Nothing, xs) _ = error "assertion fail : normalize 1"
        block b = tell [b]
        normBlock cs = reverse cs

        optJumps bs = hd bs ++ optJumps' (split (tl bs))
          where
                hd (h@(_, [CmdJmp _ _]):_) = [h]
                hd _ = []
                tl  (h@(_, [CmdJmp _ _]):xs) = xs
                tl _ = bs
                p (n, [CmdJmp _ _]) = False
                p _                 = True
                split bs            = partition p bs
                optJumps' (xs, [])  = xs
                optJumps' (gs, ((n,[CmdJmp o (ALabel n')]):ss)) = optJumps' (replJump n n' gs, replJump n n' ss)
                optJumps' (gs, x:xs) = optJumps' (gs ++ [x], xs)
                replJump n n1 = map (\(l, xs) -> (l, map (replJump' n n1) xs))
                replJump' n n1 (CmdJmp op (ALabel n')) | n == n' = CmdJmp op (ALabel n1)
                replJump' n n1 (CmdCondJmp op (ALabel n')) | n == n' = CmdCondJmp op (ALabel n1)
                replJump' _ _ x = x


        optBlock (l, cmd) = (l, execWriter (opt cmd))
          where opt (Cmd0 EXIT:xs) = tell [Cmd0 EXIT]
                opt (x:xs) = tell [x] >> opt xs
                opt []     = return ()

--        mergeBlocks bs = trace (show jmap) $ map block (filter (not . (flip M.member jmap . fst)) bs)
        mergeBlocks bs = map block (filter (not . (flip M.member jmap . fst)) bs)
          where
            jmap = M.filter (==1) $ flip execState M.empty $ mapM_ jmp (concat (map snd bs))
--            jmap = flip execState M.empty $ mapM_ jmp (concat (map snd bs))
            jmp (CmdJmp _ (ALabel n)) = lookSt n >>= inc n
            jmp (CmdCondJmp _ (ALabel n)) = modify (M.insert n 2)
            jmp (Cmd1 CALL (ADDR (ALabel n))) = modify (M.insert n 2)
            jmp _ = return ()
            lookSt n = gets (M.lookup n)
            inc n (Nothing) = modify (M.insert n 1)
            inc n (Just v)  = modify (M.adjust (succ) n)
            bmap = M.fromList bs
            block (l, cmds) = (l, execWriter (mapM_ (cmd l) cmds))
            cmd l (CmdJmp op (ALabel n)) | M.member n jmap = tellB l n
            cmd l x = tell [x]
            tellB l n = tell (snd (block (l, fromJust (M.lookup n bmap))))


