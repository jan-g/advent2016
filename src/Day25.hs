module Day25 where

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
import Data.Either

import Debug.Trace (trace)

import Lib


{-
--- Day 25: Clock Signal ---

You open the door and find yourself on the roof. The city sprawls away from you for miles and miles.

There's not much time now - it's already Christmas, but you're nowhere near the North Pole, much too far to deliver these stars to the sleigh in time.

However, maybe the huge antenna up here can offer a solution. After all, the sleigh doesn't need the stars, exactly; it needs the timing data they provide, and you happen to have a massive signal generator right here.

You connect the stars you have to your prototype computer, connect that to the antenna, and begin the transmission.

Nothing happens.

You call the service number printed on the side of the antenna and quickly explain the situation. "I'm not sure what kind of equipment you have connected over there," he says, "but you need a clock signal." You try to explain that this is a signal for a clock.

"No, no, a clock signal - timing information so the antenna computer knows how to read the data you're sending it. An endless, alternating pattern of 0, 1, 0, 1, 0, 1, 0, 1, 0, 1...." He trails off.

You ask if the antenna can handle a clock signal at the frequency you would need to use for the data from the stars. "There's no way it can! The only antenna we've installed capable of that is on top of a top-secret Easter Bunny installation, and you're definitely not-" You hang up the phone.

You've extracted the antenna's clock signal generation assembunny code (your puzzle input); it looks mostly compatible with code you worked on just recently.

This antenna code, being a signal generator, uses one extra instruction:

    out x transmits x (either an integer or the value of a register) as the next value for the clock signal.

The code takes a value (via register a) that describes the signal to generate, but you're not sure how it's used. You'll have to find the input to produce the right signal through experimentation.

What is the lowest positive integer that can be used to initialize register a and cause the code to output a clock signal of 0, 1, 0, 1... repeating forever?

To begin, get your puzzle input.
-}

type Reg = Char
type Value = Integer

data Instr = Cpy Source Source | Inc Source | Dec Source | Jnz Source Source
            -- added for day 25
           | Out Source
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
             (string "out " *> pure Out <*> parseSource)

run :: Set.Set (Integer, Map.Map Reg Value) -> Map.Map Integer Instr -> Integer -> Map.Map Reg Value
    -> [Either (Value, Integer, Map.Map Reg Value)
               (Maybe (Value, Integer, Map.Map Reg Value))]
run seen prog pc regs =
--  trace ("pc=" ++ show pc) $
  case Map.lookup pc prog of
    Nothing      -> [Right Nothing]
    Just (Inc (Reg r)) -> run seen prog (succ pc) (store r (reg r + 1))
    Just (Inc _) -> run seen prog (succ pc) regs
    Just (Dec (Reg r)) -> run seen prog (succ pc) (store r (reg r - 1))
    Just (Dec _) -> run seen prog (succ pc) regs
    Just (Cpy s (Reg t)) -> run seen prog (succ pc) (store t (eval s))
    Just (Cpy _ _) -> run seen prog (succ pc) regs
    Just (Jnz s v) -> if eval s == 0
                      then run seen prog (succ pc) regs
                      else run seen prog (pc + eval v) regs
    Just (Out s) -> if Set.member (pc, regs) seen then [Right $ Just (eval s, pc, regs)]
                    else (Left (eval s, pc, regs)) : run (Set.insert (pc, regs) seen) prog (succ pc) regs
  where
    eval (Value v) = v
    eval (Reg r) = reg r
    reg r = fromMaybe 0 (Map.lookup r regs)
    store r v = Map.insert r v regs


day25 ls = hunt 1
--  run Set.empty prog 0 $ Map.singleton 'a' 158
  where
    prog = parse ls
    sequence n = take 1000 $ run Set.empty prog 0 $ Map.singleton 'a' n
    repetition n = let s = sequence n
                       front = init s
                       final = last s
                       Left (_, pc0, reg0) = head s
                   in  -- Does this loop back to the start?
                   case final of
                     Right (Just (_, pc1, reg1)) -> if pc1 == pc0 && reg1 == reg0
                                                    then Just $ valuesOf front
                                                    else Nothing
                     _ -> Nothing
    valuesOf items = items & map (\(Left (out, _, _)) -> out)
    target = cycle [0, 1]
    hunt n =
      trace ("checking " ++ show n) $
      case repetition n of
        Nothing -> hunt (succ n)
        Just values -> if values == take (length values) target then n else hunt (succ n)

{-
--- Part Two ---

The antenna is ready. Now, all you need is the fifty stars required to generate the signal for the sleigh, but you don't have enough.

You look toward the sky in desperation... suddenly noticing that a lone star has been installed at the top of the antenna! Only 49 more to go.

You have enough stars to transmit the signal.

Although it hasn't changed, you can still get your puzzle input.
-}

day25b ls = "hello world"
