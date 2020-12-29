module Day2 where

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
--- Day 2: Bathroom Security ---

You arrive at Easter Bunny Headquarters under cover of darkness. However, you left in such a rush that you forgot to use the bathroom! Fancy office buildings like this one usually have keypad locks on their bathrooms, so you search the front desk for the code.

"In order to improve security," the document you find says, "bathroom codes will no longer be written down. Instead, please memorize and follow the procedure below to access the bathrooms."

The document goes on to explain that each button to be pressed can be found by starting on the previous button and moving to adjacent buttons on the keypad: U moves up, D moves down, L moves left, and R moves right. Each line of instructions corresponds to one button, starting at the previous button (or, for the first line, the "5" button); press whatever button you're on at the end of each line. If a move doesn't lead to a button, ignore it.

You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. You picture a keypad like this:

1 2 3
4 5 6
7 8 9

Suppose your instructions are:

ULL
RRDDD
LURDL
UUUUD

    You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and stay on "1"), so the first button is 1.
    Starting from the previous button ("1"), you move right twice (to "3") and then down three times (stopping at "9" after two moves and ignoring the third), ending up with 9.
    Continuing from "9", you move left, up, right, down, and left, ending with 8.
    Finally, you move up four times (stopping at "2"), then down once, ending with 5.

So, in this example, the bathroom code is 1985.

Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?
-}

parse ls = ls

grid = Map.fromList [((-1, 1), 1), ((0, 1), 2), ((1, 1), 3),
                     ((-1, 0), 4), ((0, 0), 5), ((1, 0), 6),
                     ((-1,-1), 7), ((0,-1), 8), ((1,-1), 9)]

dir 'U' = (0, 1)
dir 'D' = (0, -1)
dir 'L' = (-1, 0)
dir 'R' = (1, 0)

move (x, y) d = let (dx, dy) = dir d
                    (x', y') = (x + dx, y + dy)
                in  if Map.member (x', y') grid then (x', y') else (x, y)

follow (x0, y0) line = foldl move (x0, y0) line

followAll instrs = foldl (\(acc, pos) line -> let pos' = follow pos line in (acc ++ [pos'], pos')) ([], (0, 0)) instrs 

decode instrs = let (path, _) = followAll instrs in map (grid Map.!) path

day2 ls = decode ls

{-
-}

day2b ls = "hello world"
