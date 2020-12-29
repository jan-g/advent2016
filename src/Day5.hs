module Day5 where

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
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU
import qualified Text.Hex as Hex

import Lib


{-
--- Day 5: How About a Nice Game of Chess? ---

You are faced with a security door designed by Easter Bunny engineers that seem to have acquired most of their security knowledge by watching hacking movies.

The eight-character password for the door is generated one character at a time by finding the MD5 hash of some Door ID (your puzzle input) and an increasing integer index (starting with 0).

A hash indicates the next character in the password if its hexadecimal representation starts with five zeroes. If it does, the sixth character in the hash is the next character of the password.

For example, if the Door ID is abc:

    The first index which produces a hash that starts with five zeroes is 3231929, which we find by hashing abc3231929; the sixth character of the hash, and thus the first character of the password, is 1.
    5017308 produces the next interesting hash, which starts with 000008f82..., so the second character of the password is 8.
    The third time a hash starts with five zeroes is for abc5278568, discovering the character f.

In this example, after continuing this search a total of eight times, the password is 18f47a30.

Given the actual Door ID, what is the password?
-}

parse :: [String] -> String
parse ls = ls & head

toChar 0 = '0'
toChar 1 = '1'
toChar 2 = '2'
toChar 3 = '3'
toChar 4 = '4'
toChar 5 = '5'
toChar 6 = '6'
toChar 7 = '7'
toChar 8 = '8'
toChar 9 = '9'
toChar 10 = 'a'
toChar 11 = 'b'
toChar 12 = 'c'
toChar 13 = 'd'
toChar 14 = 'e'
toChar 15 = 'f'

hash s n = let a:b:c:_ = MD5.hash (BSU.fromString $ s ++ show n) & B.unpack
           in  if a == 0 && b == 0 && c < 16 then Just $ toChar c else Nothing

hunt s n = case hash s n of
             Nothing -> hunt s (n + 1)
             Just c  -> c : hunt s (n + 1)

pwd s = take 8 (hunt s 0)

day5 ls = let k = parse ls in pwd k

{-
-}

day5b ls = "hello world"
