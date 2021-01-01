module Day20 where

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
import Data.Heap as Heap

import Lib


{-
--- Day 20: Firewall Rules ---

You'd like to set up a small hidden computer here so you can use it to get back into the network later. However, the corporate firewall only allows communication with certain external IP addresses.

You've retrieved the list of blocked IPs from the firewall, but the list seems to be messy and poorly maintained, and it's not clear which IPs are allowed. Also, rather than being written in dot-decimal notation, they are written as plain 32-bit integers, which can have any value from 0 through 4294967295, inclusive.

For example, suppose only the values 0 through 9 were valid, and that you retrieved the following blacklist:

5-8
0-2
4-7

The blacklist specifies ranges of IPs (inclusive of both the start and end value) that are not allowed. Then, the only IPs that this firewall allows are 3 and 9, since those are the only numbers not in any range.

Given the list of blocked IPs you retrieved from the firewall (your puzzle input), what is the lowest-valued IP that is not blocked?


To begin, get your puzzle input.
-}

parse :: [String] -> [(Integer, Integer)]
parse ls = ls
         & map (\l -> let [a, b] = splitOn "-" l in (read a, read b))

day20 ls = let ranges = parse ls
               increases = [ (a, -1) | (a, b) <- ranges ]
               decreases = [ (b+1, 1) | (a, b) <- ranges ]
               queue = Heap.fromList (increases ++ decreases)  :: Heap.MinHeap (Integer, Integer)
           in  process 0 queue

process :: Integer -> Heap.MinHeap (Integer, Integer) -> Maybe Integer
process level queue
  | Heap.null queue = Nothing
  | otherwise = let Just ((ip, reduction), rest) = Heap.view queue
                    level' = level - reduction    -- so increases come first
                in  if level' == 0 then Just ip
                    else process level' rest

{-
--- Part Two ---

How many IPs are allowed by the blacklist?
-}

rangesToHeap ranges = let increases = [ (a, -1) | (a, b) <- ranges ]
                          decreases = [ (b+1, 1) | (a, b) <- ranges ]
                      in  Heap.fromList (increases ++ decreases)  :: Heap.MinHeap (Integer, Integer)

day20b ls = let ranges = parse ls
                queue = rangesToHeap ranges
            in  process' 4294967296 0 0 0 queue

process' :: Integer -> Integer -> Integer -> Integer -> Heap.MinHeap (Integer, Integer) -> Integer
process' top count curLow level queue
  | Heap.null queue = count + max 0 (top - curLow)
  | otherwise = let Just ((ip, reduction), rest) = Heap.view queue
                    level' = level - reduction
                    count' = if level == 0 then count + (ip - curLow) else count
                    curLow' = if level' == 0 then ip else curLow
                    in process' top count' curLow' level' rest
