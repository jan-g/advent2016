module Day23 where

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

import Debug.Trace (trace)

import Lib


{-
--- Day 23: Safe Cracking ---

This is one of the top floors of the nicest tower in EBHQ. The Easter Bunny's private office is here, complete with a safe hidden behind a painting, and who wouldn't hide a star in a safe behind a painting?

The safe has a digital screen and keypad for code entry. A sticky note attached to the safe has a password hint on it: "eggs". The painting is of a large rabbit coloring some eggs. You see 7.

When you go to type the code, though, nothing appears on the display; instead, the keypad comes apart in your hands, apparently having been smashed. Behind it is some kind of socket - one that matches a connector in your prototype computer! You pull apart the smashed keypad and extract the logic circuit, plug it into your computer, and plug your computer into the safe.

Now, you just need to figure out what output the keypad would have sent to the safe. You extract the assembunny code from the logic chip (your puzzle input).

The code looks like it uses almost the same architecture and instruction set that the monorail computer used! You should be able to use the same assembunny interpreter for this as you did there, but with one new instruction:

tgl x toggles the instruction x away (pointing at instructions like jnz does: positive means forward; negative means backward):

    For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
    For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
    The arguments of a toggled instruction are not affected.
    If an attempt is made to toggle an instruction outside the program, nothing happens.
    If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
    If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed until the next time it is reached.

For example, given this program:

cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a

    cpy 2 a initializes register a to 2.
    The first tgl a toggles an instruction a (2) away from it, which changes the third tgl a into inc a.
    The second tgl a also modifies an instruction 2 away from it, which changes the cpy 1 a into jnz 1 a.
    The fourth line, which is now inc a, increments a to 3.
    Finally, the fifth line, which is now jnz 1 a, jumps a (3) instructions ahead, skipping the dec a instructions.

In this example, the final value in register a is 3.

The rest of the electronics seem to place the keypad entry (the number of eggs, 7) in register a, run the code, and then send the value left in register a to the safe.

What value should be sent to the safe?

To begin, get your puzzle input.
-}

type Reg = Char
type Value = Integer

data Instr = Cpy Source Source | Inc Source | Dec Source | Jnz Source Source | Tgl Source
            -- these added for part 2
           | Nop | Mul Reg Reg Reg
  deriving (Show, Eq)

data Source = Reg Reg | Value Value deriving (Show, Eq)

parse :: [String] -> Map.Map Integer Instr
parse ls = ls
         & map (quickParse $ parseInstr <* eof)
         & catMaybes
         & zip [0..]
         & Map.fromList

parseReg = satisfy isAlpha
parseSource = (Reg <$> parseReg) <++ (Value <$> intParser)
parseInstr = (string "inc " *> pure Inc <*> parseSource) <++
             (string "dec " *> pure Dec <*> parseSource) <++
             (string "cpy " *> pure Cpy <*> parseSource <* char ' ' <*> parseSource) <++
             (string "jnz " *> pure Jnz <*> parseSource <* char ' ' <*> parseSource) <++
             (string "tgl " *> pure Tgl <*> parseSource)

run :: Map.Map Integer Instr -> Integer -> Map.Map Reg Value -> Map.Map Reg Value
run prog pc regs =
--  trace ("pc=" ++ show pc) $
  case Map.lookup pc prog of
    Nothing      -> regs
    Just (Inc (Reg r)) -> run prog (succ pc) (store r (reg r + 1))
    Just (Inc _) -> run prog (succ pc) regs
    Just (Dec (Reg r)) -> run prog (succ pc) (store r (reg r - 1))
    Just (Dec _) -> run prog (succ pc) regs
    Just (Cpy s (Reg t)) -> run prog (succ pc) (store t (eval s))
    Just (Cpy _ _) -> run prog (succ pc) regs
    Just (Jnz s v) -> if eval s == 0
                      then run prog (succ pc) regs
                      else run prog (pc + eval v) regs
    Just (Tgl s) -> run (tgl (eval s)) (succ pc) regs
    Just (Nop) -> run prog (succ pc) regs
    Just (Mul a b c) -> run prog (succ pc) (store c (reg a * reg b))
  where
    eval (Value v) = v
    eval (Reg r) = reg r
    reg r = fromMaybe 0 (Map.lookup r regs)
    store r v = Map.insert r v regs
    tgl v = case Map.lookup (pc + v) prog of
              Nothing -> prog
              Just (Inc r) -> Map.insert (pc + v) (Dec r) prog
              Just (Dec r) -> Map.insert (pc + v) (Inc r) prog
              Just (Cpy s t) -> Map.insert (pc + v) (Jnz s t) prog
              Just (Jnz s t) -> Map.insert (pc + v) (Cpy s t) prog
              Just (Tgl s) ->  Map.insert (pc + v) (Inc s) prog
              

day23 ls = run (parse ls) 0 (Map.singleton 'a' 7)
         & (Map.! 'a')

{-
--- Part Two ---

The safe doesn't open, but it does make several angry noises to express its frustration.

You're quite sure your logic is working correctly, so the only other thing is... you check the painting again. As it turns out, colored eggs are still eggs. Now you count 12.

As you run the program with this new input, the prototype computer begins to overheat. You wonder what's taking so long, and whether the lack of any instruction more powerful than "add one" has anything to do with it. Don't bunnies usually multiply?

Anyway, what value should actually be sent to the safe?

-}

replace prog seq1 seq2 =
  prog & Map.toAscList & map snd & replace' & zip [0..] & Map.fromList
  where
    len = length seq1 
    replace' as
     | null as     = []
     | take len as == seq1 = seq2 ++ (drop len as)
     | otherwise   = (head as):replace' (tail as) 

day23b ls =
  let prog = parse ls
      prog' = replace prog [ Cpy (Value 0) (Reg 'a')
                           , Cpy (Reg 'b') (Reg 'c')
                           , Inc (Reg 'a')
                           , Dec (Reg 'c')
                           , Jnz (Reg 'c') (Value (-2))
                           , Dec (Reg 'd')
                           , Jnz (Reg 'd') (Value (-5))
                           ]
                           [ Nop
                           , Nop
                           , Nop
                           , Nop
                           , Nop
                           , Nop
                           , Mul 'b' 'd' 'a'
                           ]
                           
  in  run prog' 0 (Map.singleton 'a' 12)
                   & (Map.! 'a')

{-
cpy a b           b = a               b = a = 12
dec b        B    b--                 b = 11
cpy a d      D    E: d = a            d = 12   132
cpy 0 a           a = 0               a = 0
cpy b c           A: c = b            c = 11
inc a             B: a++              
dec c             c--                   ..
jnz c -2          if c != 0 jmp B       a += c, c=0
dec d             d--
jnz d -5     A    if d != 0 jmp A     a = d * b  a = 132
                                      d=0 c=0
dec b        B    b--                 b = 10 9 8 7 6 5 4 3 2 1
cpy b c      C    c = b               c = 10
cpy c d      D    d = c               d = 10
dec d             C: d--
inc c             c++
jnz d -2          if d != 0 jmp C     c = 2b
tgl c             tgl c               d=0, c=2b, a=massive
cpy -16 c         c = -16             c = -16
jnz 1 c           jmp E: -> 
cpy 76 c
jnz 84 d
inc a
inc d
jnz d -2
inc c
jnz c -5
-}