module BF where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

type Zipper a = ([a], a, [a])
type Cells a  = Zipper a
type Program = Cells Char
type Memory  = Cells Word8

-- Transform program string into cells.
initProgram :: String -> Program
initProgram (x:xs) = ([], x, xs)

-- Initialize infinite memory (bounded by memory) with 0's.
initMemory :: Memory
initMemory = ([], 0, repeat 0)

-------------------------------------
------ Core Brainfuck Commands ------
-------------------------------------
---
-- increment the data pointer (to the point to the next cell to the right)
(^>) :: Cells a -> Cells a
(^>) cs@(_, _, []) = cs
(^>) (ls, f, r:rs) = (f:ls, r, rs) 

-- decrement the data pointer (to the point to the next cell to the left).
(^<) :: Cells a -> Cells a
(^<) ([], _, _)    = error "(^<): hit lower bound"
(^<) (l:ls, f, rs) = (ls, l, f:rs)

-- increment the byte at the data pointer
(^+) :: Memory -> Memory
(^+) (ls, f, rs) = (ls, f + 1, rs)

-- decrement the byte at the data pointer
(^-) :: Memory -> Memory
(^-) (ls, f, rs) = (ls, f - 1, rs)

-- output the byte at the data pointer
(^.) :: Memory -> IO ()
(^.) (_, f, _) = (BL.putStr . runPut . putWord8) f

{-
(^./) :: Memory -> Memory
(^./) (ls, _, rs) = (ls, c, rs)
  where c = B.head . B.getLine
-}

-- if the byte at the data pointer is zero, then instead of moving the
-- instruction pointer forward to the next command, jump it forward to the
-- command after the matching `]` command.
-- `|-` is the same as `[`.
(^|-) :: Word8 -> Program -> Program
(^|-) df p
  | df == 0 = jumpToClose 0 p
  | otherwise = (^>) p
  where jumpToClose :: Int -> Program -> Program
        jumpToClose _ (_, _, []) = error "jumpToClose: hit upper bound"
        jumpToClose i p'@(_, f, _) = case (i, f) of
                                       (0, ']') -> p'
                                       (i', ']') -> jumpToClose (i' - 1) $ (^>) p'
                                       (i', '[') -> jumpToClose (i' + 1) $ (^>) p'

-- if the byte at the data pointer is nonzero, then instead of moving the
-- instruction pointer forward to the next command, jump it back to the
-- command after the matching `[` command.
-- `-|` is the same as `]`.
(^-|) :: Word8 -> Program -> Program
(^-|) df p
  | df /= 0 = jumpToAfterOpen 0 p
  | otherwise = (^>) p
  where jumpToAfterOpen :: Int -> Program -> Program
        jumpToAfterOpen _ ([], _, _) = error "jumpToAfterOpen: hit lower bound"
        jumpToAfterOpen i p'@(l:_, _, _) = case (i, l) of
                                       (0, '[') -> p'
                                       (i', '[') -> jumpToAfterOpen (i' + 1) $ (^<) p'
                                       (i', ']') -> jumpToAfterOpen (i' - 1) $ (^<) p'
                                       _ -> jumpToAfterOpen i $ (^<) p'
---
-------------------------------------
-------------------------------------

-- Deals with program data transformations.
applyProg :: Char -> Word8 -> Program -> Program
applyProg c f =
    case c of
      '[' -> (^|-) f
      ']' -> (^-|) f
      _   -> (^>)

-- Deals with memory data transformations.
applyMem :: Char -> Memory -> Memory
applyMem c =
    case c of
      '>' -> (^>)
      '<' -> (^<)
      '+' -> (^+)
      '-' -> (^-)
      _   -> id

processCommand :: Char -> Program -> Memory -> IO (Program, Memory)
processCommand '.' p m = do
    _ <- (^.) m
    return ((^>) p, m)
processCommand c p m@(_, f, _) = return (applyP p, applyM m)
  where applyP = applyProg c f
        applyM = applyMem c

execute :: Program -> Memory -> IO (Program, Memory)
execute p@(_, f, []) m = processCommand f p m
execute p@(_, f, _) m = do
    (p', m') <- processCommand f p m
    execute p' m'

main :: IO ()
main = do
    program <- getContents
    _ <- execute (initProgram program) initMemory
    return ()
