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
import qualified Data.Sequence as Seq

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
--- Part Two ---

Realizing the folly of their present-exchange rules, the Elves agree to instead steal presents from the Elf directly across the circle. If two Elves are across the circle, the one on the left (from the perspective of the stealer) is stolen from. The other rules remain unchanged: Elves with no presents are removed from the circle entirely, and the other elves move in slightly to keep the circle evenly spaced.

For example, with five Elves (again numbered 1 to 5):

    The Elves sit in a circle; Elf 1 goes first:

      1
    5   2
     4 3

    Elves 3 and 4 are across the circle; Elf 3's present is stolen, being the one to the left. Elf 3 leaves the circle, and the rest of the Elves move in:

      1           1
    5   2  -->  5   2
     4 -          4

    Elf 2 steals from the Elf directly across the circle, Elf 5:

      1         1
    -   2  -->     2
      4         4

    Next is Elf 4 who, choosing between Elves 1 and 2, steals from Elf 1:

     -          2
        2  -->
     4          4

    Finally, Elf 2 steals from Elf 4:

     2
        -->  2
     -

So, with five Elves, the Elf that sits starting in position 2 gets all the presents.

With the number of Elves given in your puzzle input, which Elf now gets all the presents?
-}

part2 :: Seq.Seq a -> a
part2 xs =
  case Seq.viewl xs of
    x Seq.:< ys
      | Seq.null ys -> x
      | otherwise   -> part2 ((l Seq.>< Seq.drop 1 r) Seq.|> x)
          where (l,r) = Seq.splitAt (half (length ys)) ys
                half x = (x-1) `div` 2

part2' head preOpposite !elves =
  if Map.size elves == 1 then elves
  else -- drop the opposite
       let opp = elves Map.! preOpposite
           oppl = elves Map.! opp
           elves' = elves & Map.delete opp
                          & Map.insert preOpposite oppl
           left = elves' Map.! head
       in if even (Map.size elves) then part2' left oppl elves'
                                   else part2' left (elves' Map.! oppl) elves'
               
setup' n = (1, n `div` 2, Map.fromList [(i + 1, (i + 1) `mod` n + 1) | i <- [0..n-1]])


day19b ls =
  let n = parse ls
      (head, preopp, elves) = setup' n
  in  part2 (Seq.fromList [1..n])
      -- part2' head preopp elves
