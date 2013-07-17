module Brainfuck where

import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import qualified System.IO as IO
import System.Environment (getArgs)

-- Zipper :: (left side (reverse, nearest to focus is at head), current focus, right side)
type Zipper a = ([a], a, [a]) 
type Cells a  = Zipper a
type Program = Cells Char
type Memory  = Cells Word8

-- Transform program string into cells.
initProgram :: String -> Program
initProgram (x:xs) = (" ", x, xs ++ " ")

-- Initialize infinite memory (bounded by memory) with 0's.
initMemory :: Memory
initMemory = ([], 0, repeat 0)

-------------------------------------
------ Core Brainfuck Commands ------
-------------------------------------
---
-- `>` | increment the data pointer (to the point to the next cell to the right)
(^>) :: Cells a -> Cells a
(^>) cs@(_, _, []) = error "(^>): hit upper bound"
(^>) (ls, f, r:rs) = (f:ls, r, rs) 

-- `<` | decrement the data pointer (to the point to the next cell to the left).
(^<) :: Cells a -> Cells a
(^<) cs@([], _, _) = error "(^<): hit lower bound"
(^<) (l:ls, f, rs) = (ls, l, f:rs)

-- `+` | increment the byte at the data pointer
(^+) :: Memory -> Memory
(^+) (ls, f, rs) = (ls, f + 1, rs)

-- `-` | decrement the byte at the data pointer
(^-) :: Memory -> Memory
(^-) (ls, f, rs) = (ls, f - 1, rs)

-- `.` | output the byte at the data pointer
(^.) :: Memory -> IO ()
(^.) (_, f, _) = (BL.putStr . runPut . putWord8) f

-- `,` | accept one byte of input, storing its value in the byte at the
-- data pointer.
(^./) :: Memory -> IO Memory
(^./) (ls, _, rs) = B.hGet IO.stdin 1 >>= \b -> case B.unpack b of
                                                [byte] -> return (ls, byte, rs)
                                                _      -> error "could not read a byte of input"

-- `[` | if the byte at the data pointer is zero, then instead of moving the
-- instruction pointer forward to the next command, jump it forward to the
-- command after the matching `]` command.
(^|-) :: Word8 -> Program -> Program
(^|-) df p
  | df == 0 = jumpToClose 0 p
  | otherwise = (^>) p
  where jumpToClose :: Int -> Program -> Program
        jumpToClose _ (_, _, []) = error "jumpToClose: hit upper bound"
        jumpToClose i p'@(_, f, _) = case (i, f) of
                                       (1, ']') -> p'
                                       (_', ']') -> jumpToClose (i - 1) $ (^>) p'
                                       (_', '[') -> jumpToClose (i + 1) $ (^>) p'
                                       _ -> jumpToClose i $ (^>) p'

-- `]` | if the byte at the data pointer is nonzero, then instead of moving the
-- instruction pointer forward to the next command, jump it back to the
-- command after the matching `[` command.
(^-|) :: Word8 -> Program -> Program
(^-|) df p
  | df /= 0 = jumpToAfterOpen 0 p
  | otherwise = (^>) p
  where jumpToAfterOpen :: Int -> Program -> Program
        jumpToAfterOpen _ ([], _, _) = error "jumpToAfterOpen: hit lower bound"
        jumpToAfterOpen i p'@(l:_, _, _) = case (i, l) of
                                       (0, '[') -> p'
                                       (_, '[') -> jumpToAfterOpen (i + 1) $ (^<) p'
                                       (_', ']') -> jumpToAfterOpen (i - 1) $ (^<) p'
                                       _ -> jumpToAfterOpen i $ (^<) p'
---
-------------------------------------
-------------------------------------

-- Deals with program data transformations.
transformProg :: Program -> Memory -> Program -> Program
transformProg (_, ip, _) (_, dp, _) =
    case ip of
      '[' -> (^|-) dp
      ']' -> (^-|) dp
      _   -> (^>)

-- Deals with memory data transformations.
transformMem :: Program -> Memory -> Memory
transformMem (_, ip, _) =
    case ip of
      '>' -> (^>)
      '<' -> (^<)
      '+' -> (^+)
      '-' -> (^-)
      _   -> id

execute :: (Program, Memory) -> IO (Program, Memory)
execute (p@(_, ip, rs), m)
  | null rs = return (p, m)
  | ip == '.' = (^.) m >> execute state
  | ip == ',' = (^./) m >>= \m' -> execute (applyP p, m')
  | otherwise = execute state >>= execute
  where applyP = transformProg p m
        applyM = transformMem p
        state = (applyP p, applyM m)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "Usage: runhaskell bf.hs <program file>"
      f:_ -> do
        program <- readFile f
        _ <- execute (initProgram program, initMemory)
        return ()
