module Day24 where

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
--- Day 24: Air Duct Spelunking ---

You've finally met your match; the doors that provide access to the roof are locked tight, and all of the controls and related electronics are inaccessible. You simply can't reach them.

The robot that cleans the air ducts, however, can.

It's not a very fast little robot, but you reconfigure it to be able to interface with some of the exposed wires that have been routed through the HVAC system. If you can direct it to each of those locations, you should be able to bypass the security controls.

You extract the duct layout for this area from some blueprints you acquired and create a map with the relevant locations marked (your puzzle input). 0 is your current location, from which the cleaning robot embarks; the other numbers are (in no particular order) the locations the robot needs to visit at least once each. Walls are marked as #, and open passages are marked as .. Numbers behave like open passages.

For example, suppose you have a map like the following:

###########
#0.1.....2#
#.#######.#
#4.......3#
###########

To reach all of the points of interest as quickly as possible, you would have the robot take the following path:

    0 to 4 (2 steps)
    4 to 1 (4 steps; it can't move diagonally)
    1 to 2 (6 steps)
    2 to 3 (2 steps)

Since the robot isn't very fast, you need to find it the shortest route. This path is the fewest steps (in the above example, a total of 14) required to start at 0 and then visit every other location at least once.

Given your actual map, and starting from location 0, what is the fewest number of steps required to visit every non-0 number marked on the map at least once?

To begin, get your puzzle input.
-}

parse ls = loadMap ls

swap (a, b) = (b, a)

day24 ls =
  let m = parse ls
      digits = Map.filter isDigit m & Map.toList & map swap  -- :: Map.Map Char (Integer, Integer)
      distances = Map.fromList [(a,
                   [(b, distance m ca cb) | (b, cb) <- digits, b /= a])
                   | (a, ca) <- digits]
  in bfs (next distances) (finished distances) id (0, ('0', Set.singleton '0'))
  where
    finished distances (_, (_, visited)) = Set.size visited == length distances
    next distances (c, (at, visited)) = let dests = distances Map.! at
                                        in Set.fromList [ (c+d, (dest, Set.insert dest visited))
                                                        | (dest, d) <- dests
                                                        ]


type Coord = (Integer, Integer)

distance :: Map.Map Coord Char -> Coord -> Coord -> Integer
distance maze from to =
  let Just (cost, _) = bfs next finished id (0, from)
  in cost
  where
    finished (_, x) = x == to
    next (c, (x, y)) = Set.fromList [(succ c, (x', y')) | (dx, dy) <- [(-1,0), (1,0), (0,-1), (0, 1)],
                                                          let (x', y') = (x+dx, y+dy),
                                                          maze Map.! (x', y') /= '#']

{-
--- Part Two ---

Of course, if you leave the cleaning robot somewhere weird, someone is bound to notice.

What is the fewest number of steps required to start at 0, visit every non-0 number marked on the map at least once, and then return to 0?
-}

day24b ls =
  let m = parse ls
      digits = Map.filter isDigit m & Map.toList & map swap  -- :: Map.Map Char (Integer, Integer)
      distances = Map.fromList [(a,
                   Map.fromList [(b, distance m ca cb) | (b, cb) <- digits, b /= a])
                   | (a, ca) <- digits]
  in bfs (next distances) (finished distances) id (0, ('0', Set.singleton '0', False))
  where
    finished distances (_, (_, _, circled)) = circled
    next distances (c, (at, visited, False))
      | Set.size visited == length distances = Set.singleton (c + distances Map.! at Map.! '0', ('0', visited, True))
      | otherwise = let dests = distances Map.! at
                    in Set.fromList [ (c+d, (dest, Set.insert dest visited, False))
                             | (dest, d) <- Map.toList dests
                             ]
