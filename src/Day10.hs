{-# LANGUAGE BangPatterns #-}

module Day10 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))
import Debug.Trace (trace)

import Lib


{-
--- Day 10: Balance Bots ---

You come upon a factory in which many robots are zooming around handing small microchips to each other.

Upon closer examination, you notice that each bot only proceeds when it has two microchips, and once it does, it gives each one to a different bot or puts it in a marked "output" bin. Sometimes, bots take microchips from "input" bins, too.

Inspecting one of the microchips, it seems like they each contain a single number; the bots must use some logic to decide what to do with each chip. You access the local control computer and download the bots' instructions (your puzzle input).

Some of the instructions specify that a specific-valued microchip should be given to a specific bot; the rest of the instructions indicate what a given bot should do with its lower-value or higher-value chip.

For example, consider the following instructions:

value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2

    Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2 chip and a value-5 chip.
    Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its higher one (5) to bot 0.
    Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives the value-3 chip to bot 0.
    Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in output 0.

In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a value-2 microchip, and output bin 2 contains a value-3 microchip. In this configuration, bot number 2 is responsible for comparing value-5 microchips with value-2 microchips.

Based on your instructions, what is the number of the bot that is responsible for comparing value-61 microchips with value-17 microchips?

To begin, get your puzzle input.
-}

type Const = Integer
type Bot = Integer
type Output = Integer
data Dest = Bot Bot | Output Output deriving (Show, Eq)
data Instr = Value Const Bot | Give Bot Dest Dest deriving (Show, Eq)

parse ls = ls
         & map (quickParse parseInstr)
         & catMaybes

parseInstr = (parseValue <++ parseGive) <* eof

parseValue = do
  string "value "
  v <- natParser
  string " goes to bot "
  b <- natParser
  return $ Value v b

parseGive = do
  string "bot "
  b <- natParser
  string " gives low to "
  d1 <- parseDest
  string " and high to "
  d2 <- parseDest
  return $ Give b d1 d2

parseDest = (string "output " *> pure Output <*> natParser) <++
            (string "bot " *> pure Bot <*> natParser)

data State = State { bots :: Map.Map Bot [Const]
                   , outputs :: Map.Map Output Const
                   , valueAssignments :: [Instr]
                   , botAssignments :: Map.Map Bot Instr
                   , botsReady :: [Bot]
                   , comparisons :: Map.Map (Const, Const) Bot
                   }
  deriving (Show, Eq)

isValueAssignment (Value _ _) = True
isValueAssignment _ = False

makeState is =
  let (vs, bs) = partition isValueAssignment is
      bs' = Map.fromList [(b, i) | i@(Give b _ _) <- bs]
  in  State { bots=Map.empty, outputs=Map.empty, valueAssignments=vs, botAssignments=bs', botsReady=[], comparisons=Map.empty }

run !s
  -- process all value assignments first
  | not $ null (valueAssignments s) = -- trace (show $ head (valueAssignments s)) $
                                      let (Value v b) = head $ valueAssignments s
                                          s' = s { valueAssignments=tail (valueAssignments s) }
                                          s2 = give s' v b
                                      in  -- trace ("  new state" ++ (show s2)) $ 
                                          run s2
  -- process bot assignments next
  | not $ null (botsReady s)        = -- trace ("activating bot " ++ show (head $ botsReady s)) $
                                      let b = head (botsReady s)
                                          s' = s { botsReady=tail (botsReady s) }
                                          (Give _ d1 d2) = (botAssignments s') Map.! b
                                          [v1, v2] = (bots s') Map.! b
                                          (low, high) = (v1 `min` v2, v1 `max` v2)
                                          s2 = move s' low d1
                                          s3 = move s2 high d2
                                      in -- trace ("  destinations: " ++ (show d1) ++ " and " ++ (show d2)) $
                                         -- trace ("  new state: " ++ show s3) $ 
                                         run s3 { comparisons=Map.insert (low, high) b (comparisons s3) }
  | otherwise         = s
  where
    give s v b = -- trace ("assigning " ++ (show v) ++ " to " ++ (show b)) $
                         let bots' = Map.insertWith (++) b [v] (bots s)
                             br'   = if length (bots' Map.! b) > 1 then b:(botsReady s) else (botsReady s)
                         in  s { bots=bots', botsReady=br' }
    move s v dest =
      -- trace ("sending " ++ (show v) ++ " to " ++ (show dest)) $
      case dest of
        Bot b    -> give s v b
        Output o -> -- trace ("outputting " ++ (show v) ++ " to " ++ (show o)) $
                    s { outputs=Map.insert o v (outputs s) }

day10 ls = parse ls & makeState & run & comparisons & (Map.! (17, 61))

{-
-}

day10b ls = "hello world"
