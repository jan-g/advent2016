{-# LANGUAGE BangPatterns #-}

module Day19 where

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

import Lib


{-
--- Day 19: An Elephant Named Joseph ---

The Elves contact you over a highly secure emergency channel. Back at the North Pole, the Elves are busy misunderstanding White Elephant parties.

Each Elf brings a present. They all sit in a circle, numbered starting with position 1. Then, starting with the first Elf, they take turns stealing all the presents from the Elf to their left. An Elf with no presents is removed from the circle and does not take turns.

For example, with five Elves (numbered 1 to 5):

  1
5   2
 4 3

    Elf 1 takes Elf 2's present.
    Elf 2 has no presents and is skipped.
    Elf 3 takes Elf 4's present.
    Elf 4 has no presents and is also skipped.
    Elf 5 takes Elf 1's two presents.
    Neither Elf 1 nor Elf 2 have any presents, so both are skipped.
    Elf 3 takes Elf 5's three presents.

So, with five Elves, the Elf that sits starting in position 3 gets all the presents.

With the number of Elves given in your puzzle input, which Elf gets all the presents?

Your puzzle input is 3004953.
-}

parse :: [String] -> Integer
parse ls = ls & head & read

starting n = Map.fromList [(i, (if i == n then 1 else i + 1, 1)) | i <- [1..n]]

move head !elves
  | Map.size elves == 1 = elves
  | otherwise = let (left, n) = elves Map.! head
                    (left2, m) = elves Map.! left
                in  move left2 (elves & Map.delete left & Map.insert head (left2, n + m))

day19 ls = parse ls & starting & move 1

{-
-}

day19b ls = "hello world"
