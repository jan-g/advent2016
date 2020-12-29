module Day1 where

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
--- Day 1: No Time for a Taxicab ---

Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by stars. Unfortunately, the stars have been stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time to work them out further.

The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then, follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the destination?

For example:

    Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
    R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
    R5, L5, R5, R3 leaves you 12 blocks away.

How many blocks away is Easter Bunny HQ?
-}

parse ls = ls
         & head
         & splitOn ", "


type Coord = (Integer, Integer)

move ((x,y),(dx, dy)) ('L':ds) = let (dx',dy') = (-dy,dx)
                                     dist = read ds
                                 in ((x + dist * dx', y + dist * dy'), (dx', dy'))

move ((x,y),(dx, dy)) ('R':ds) = let (dx',dy') = (dy,-dx)
                                     dist = read ds
                                 in ((x + dist * dx', y + dist * dy'), (dx', dy'))

start = ((0,0), (0,1))

run ((x0,y0), (dx0, dy0)) ms = foldl move ((x0, y0), (dx0, dy0)) ms

dist ((x,y), _) = abs x + abs y

day1 ls = parse ls
        & run start
        & dist

{-
--- Part Two ---

Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?
-}

rotate (pos, (dx,dy)) 'R' = (pos, (dy, -dx))
rotate (pos, (dx,dy)) 'L' = (pos, (-dy, dx))

forward ((x, y), (dx, dy)) = ((x + dx, y + dy), (dx, dy))

trace path = trace0 start (Set.singleton (0, 0)) path
  where
    trace0 pos visited ((d:ds):rest) =
      let dist = read ds
      in case trace1 (rotate pos d) visited dist of
          Left (pos', visited')  -> trace0 pos' visited' rest
          Right (pos', visited') -> pos'
    trace1 pos visited 0 = Left (pos, visited)
    trace1 pos visited n =
      let new = forward pos
      in  if Set.member (fst new) visited then Right (new, visited)
          else trace1 new (Set.insert (fst new) visited) (n-1)


day1b ls = parse ls
         & trace 
         & dist
