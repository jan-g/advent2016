module Day12 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Lib


{-
--- Day 12: Leonardo's Monorail ---

You finally reach the top floor of this building: a garden with a slanted glass ceiling. Looks like there are no more stars to be had.

While sitting on a nearby bench amidst some tiger lilies, you manage to decrypt some of the files you extracted from the servers downstairs.

According to these documents, Easter Bunny HQ isn't just this building - it's a collection of buildings in the nearby area. They're all connected by a local monorail, and there's another building not far from here! Unfortunately, being night, the monorail is currently not operating.

You remotely connect to the monorail control systems and discover that the boot sequence expects a password. The password-checking logic (your puzzle input) is easy to extract, but the code it uses is strange: it's assembunny code designed for the new computer you just assembled. You'll have to execute the code and get the password.

The assembunny code you've extracted operates on four registers (a, b, c, and d) that start at 0 and can hold any integer. However, it seems to make use of only a few instructions:

    cpy x y copies x (either an integer or the value of a register) into register y.
    inc x increases the value of register x by one.
    dec x decreases the value of register x by one.
    jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.

The jnz instruction moves relative to itself: an offset of -1 would continue at the previous instruction, while an offset of 2 would skip over the next instruction.

For example:

cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a

The above code would set register a to 41, increase its value by 2, decrease its value by 1, and then skip the last dec a (because a is not zero, so the jnz a 2 skips it), leaving register a at 42. When you move past the last instruction, the program halts.

After executing the assembunny code in your puzzle input, what value is left in register a?

To begin, get your puzzle input.
-}

type Reg = Char
type Value = Integer

data Instr = Cpy Source Reg | Inc Reg | Dec Reg | Jnz Source Value deriving (Show, Eq)

data Source = Reg Reg | Value Value deriving (Show, Eq)

parse :: [String] -> Map.Map Integer Instr
parse ls = ls
         & map (quickParse $ parseInstr <* eof)
         & catMaybes
         & zip [0..]
         & Map.fromList

parseReg = satisfy isAlpha
parseSource = (Reg <$> parseReg) <++ (Value <$> intParser)
parseInstr = (string "inc " *> pure Inc <*> parseReg) <++
             (string "dec " *> pure Dec <*> parseReg) <++
             (string "cpy " *> pure Cpy <*> parseSource <* char ' ' <*> parseReg) <++
             (string "jnz " *> pure Jnz <*> parseSource <* char ' ' <*> intParser)

run :: Map.Map Integer Instr -> Integer -> Map.Map Reg Value -> Map.Map Reg Value
run prog pc regs =
  case Map.lookup pc prog of
    Nothing      -> regs
    Just (Inc r) -> run prog (succ pc) (store r (reg r + 1))
    Just (Dec r) -> run prog (succ pc) (store r (reg r - 1))
    Just (Cpy s t) -> run prog (succ pc) (store t (eval s))
    Just (Jnz s v) -> if eval s == 0
                      then run prog (succ pc) regs
                      else run prog (pc + v) regs
  where
    eval (Value v) = v
    eval (Reg r) = reg r
    reg r = fromMaybe 0 (Map.lookup r regs)
    store r v = Map.insert r v regs

day12 ls =
  let prog = parse ls
      rs = run prog 0 Map.empty
  in rs Map.! 'a'

{-
--- Part Two ---

As you head down the fire escape to the monorail, you notice it didn't start; register c needs to be initialized to the position of the ignition key.

If you instead initialize register c to be 1, what value is now left in register a?
-}

day12b ls =
  let prog = parse ls
      rs = run prog 0 $ Map.singleton 'c' 1
  in rs Map.! 'a'
