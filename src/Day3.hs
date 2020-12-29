module Day3 where

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
--- Day 3: Squares With Three Sides ---

Now that you can think clearly, you move deeper into the labyrinth of hallways and office furniture that makes up this part of Easter Bunny HQ. This must be a graphic design department; the walls are covered in specifications for triangles.

Or are they?

The design document gives the side lengths of each triangle it describes, but... 5 10 25? Some of these aren't triangles. You can't help but mark the impossible ones.

In a valid triangle, the sum of any two sides must be larger than the remaining side. For example, the "triangle" given above is impossible, because 5 + 10 is not larger than 25.

In your puzzle input, how many of the listed triangles are possible?
-}

parse :: [String] -> [(Integer, Integer, Integer)]
parse ls = ls
         & map (\l -> splitOn " " l & filter (/= ""))
         & map (\[a,b,c] -> (read a, read b, read c))

orient (a,b,c) = let a' = minimum [a,b,c]
                     c' = maximum [a,b,c]
                     b' = (a + b + c) - a' - c'
                 in (a', b', c')

day3 ls = parse ls & map orient & filter (\(a,b,c) -> a + b > c) & length

{-
-}

day3b ls = "hello world"
