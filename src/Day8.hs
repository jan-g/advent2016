module Day8 where

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
--- Day 8: Two-Factor Authentication ---

You come across a door implementing what you can only assume is an implementation of two-factor authentication after a long game of requirements telephone.

To get past the door, you first swipe a keycard (no problem; there was one on a nearby desk). Then, it displays a code on a little screen, and you type that code on a keypad. Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed. After a few minutes, you've taken everything apart and figured out how it works. Now you just have to work out what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your puzzle input. The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat peculiar operations:

    rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
    rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall off the right end appear at the left end of the row.
    rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that would fall off the bottom appear at the top of the column.

For example, here is a simple sequence on a smaller screen:

    rect 3x2 creates a small rectangle in the top-left corner:

    ###....
    ###....
    .......

    rotate column x=1 by 1 rotates the second column down by one pixel:

    #.#....
    ###....
    .#.....

    rotate row y=0 by 4 rotates the top row right by four pixels:

    ....#.#
    ###....
    .#.....

    rotate column x=1 by 1 again rotates the second column down by one pixel, causing the bottom pixel to wrap back to the top:

    .#..#.#
    #.#....
    .#.....

As you can see, this display technology is extremely powerful, and will soon dominate the tiny-code-displaying-screen market. That's what the advertisement on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen did work, how many pixels should be lit?

To begin, get your puzzle input.
-}

data Instr = Rect Integer Integer | RotateRow Integer Integer | RotateCol Integer Integer deriving (Show, Eq)
type Coord = (Integer, Integer)
data M = M Coord (Map.Map Coord Char)

parse ls = ls & map (quickParse parseInstr) & catMaybes

parseInstr :: ReadP Instr
parseInstr = (parseRect <++ parseRotR <++ parseRotC) <* eof

parseRect = do
  string "rect "
  x <- natParser
  char 'x'
  y <- natParser
  return $ Rect x y

parseRotR = do
  string "rotate row y="
  y <- natParser
  string " by "
  s <- natParser
  return $ RotateRow y s

parseRotC = do
  string "rotate column x="
  x <- natParser
  string " by "
  s <- natParser
  return $ RotateCol x s


blankMap (w,h) = M (w, h) $ Map.fromList [((x, y), '.') | x <- [0..w-1], y <- [0..h-1]]

run (M wh m)    (Rect w h)        = (M wh $ Map.mapWithKey (\(x, y) c -> if x < w && y < h then '#' else c) m)
run (M (w,h) m) (RotateRow row s) = (M (w,h) $ Map.mapWithKey (\(x, y) c -> if y == row then m Map.! ((x - s) `mod` w, y) else c) m)
run (M (w,h) m) (RotateCol col s) = (M (w,h) $ Map.mapWithKey (\(x, y) c -> if x == col then m Map.! (x, (y - s) `mod` h) else c) m)

runAll m0 is = foldl run m0 is

mapFrom (M _ m) = m

day8 ls = parse ls & runAll (blankMap (50, 6)) & mapFrom & Map.filter (=='#') & Map.size

{-
-}

day8b ls = "hello world"
