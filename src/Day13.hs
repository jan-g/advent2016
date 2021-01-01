module Day13 where

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
import Data.Bits ((.&.), (.|.), popCount)

import Lib


{-
--- Day 13: A Maze of Twisty Little Cubicles ---

You arrive at the first floor of this new building to discover a much less welcoming environment than the shiny atrium of the last one. Instead, you are in a maze of twisty little cubicles, all alike.

Every location in this area is addressed by a pair of non-negative integers (x,y). Each such coordinate is either a wall or an open space. You can't move diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward positive x and y; negative values are invalid, as they represent a location outside the building. You are in a small waiting area at 1,1.

While it seems chaotic, a nearby morale-boosting poster explains, the layout is actually quite logical. You can determine whether a given x,y coordinate will be a wall or an open space using a simple system:

    Find x*x + 3*x + 2*x*y + y + y*y.
    Add the office designer's favorite number (your puzzle input).
    Find the binary representation of that sum; count the number of bits that are 1.
        If the number of bits that are 1 is even, it's an open space.
        If the number of bits that are 1 is odd, it's a wall.

For example, if the office designer's favorite number were 10, drawing walls as # and open spaces as ., the corner of the building containing 0,0 would look like this:

  0123456789
0 .#.####.##
1 ..#..#...#
2 #....##...
3 ###.#.###.
4 .##..#..#.
5 ..##....#.
6 #...##.###

Now, suppose you wanted to reach 7,4. The shortest route you could take is marked as O:

  0123456789
0 .#.####.##
1 .O#..#...#
2 #OOO.##...
3 ###O#.###.
4 .##OO#OO#.
5 ..##OOO.#.
6 #...##.###

Thus, reaching 7,4 would take a minimum of 11 steps (starting from your current location, 1,1).

What is the fewest number of steps required for you to reach 31,39?

Your puzzle input is 1364.
-}

parse :: [String] -> Integer
parse ls = ls
         & head
         & read

-- True if you *can* move there
maze gen (x, y)
  | x < 0 || y < 0 = False
  | otherwise      =
    let v = x*x + 3*x + 2*x*y + y + y*y + gen
        p = popCount v
    in p `mod` 2 == 0

next m (c, (x, y)) =
  Set.fromList [ (succ c, (a, b)) | (a, b) <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], m (a, b) ]

satisfies (tx, ty) (_, (x, y)) = tx == x && ty == y

hunt :: Integer -> (Integer, Integer) -> Maybe Integer
hunt favNum (tx, ty) =
  fst <$> bfs (next (maze favNum)) (satisfies (tx, ty)) id (0, (1, 1))

day13 ls = let n = parse ls in hunt n (31, 39)

{-
--- Part Two ---

How many locations (distinct x,y coordinates, including your starting location) can you reach in at most 50 steps?
-}

day13b ls = let n = parse ls in
  Set.size <$> flood (next (maze n)) (\(c, _) -> c > 50) id (0, (1, 1))
