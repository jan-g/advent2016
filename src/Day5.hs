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

hash s n = let a:b:c:d:_ = MD5.hash (BSU.fromString $ s ++ show n) & B.unpack
           in  if a == 0 && b == 0 && c < 16 then Just $ (c, d `div` 16) else Nothing

hunt s n = case hash s n of
             Nothing -> hunt s (n + 1)
             Just (c, d)  -> (toChar c) : hunt s (n + 1)

pwd s = take 8 (hunt s 0)

day5 ls = let k = parse ls in pwd k

{-
--- Part Two ---

As the door slides open, you are presented with a second door that uses a slightly more inspired security mechanism. Clearly unimpressed by the last version (in what movie is the password decrypted in order?!), the Easter Bunny engineers have worked out a better solution.

Instead of simply filling in the password from left to right, the hash now also indicates the position within the password to fill. You still look for hashes that begin with five zeroes; however, now, the sixth character represents the position (0-7), and the seventh character is the character to put in that position.

A hash result of 000001f means that f is the second character in the password. Use only the first result for each position, and ignore invalid positions.

For example, if the Door ID is abc:

    The first interesting hash is from abc3231929, which produces 0000015...; so, 5 goes in position 1: _5______.
    In the previous method, 5017308 produced an interesting hash; however, it is ignored, because it specifies an invalid position (8).
    The second interesting hash is at index 5357525, which produces 000004e...; so, e goes in position 4: _5__e___.

You almost choke on your popcorn as the final character falls into place, producing the password 05ace8e3.

Given the actual Door ID and this new method, what is the password? Be extra proud of your solution if it uses a cinematic "decrypting" animation.
-}

scan sz m s n = case hash s n of
              Nothing -> scan sz m s (n + 1)
              Just (c, d) -> if fromIntegral c >= 8 then scan sz m s (n + 1)
                             else let m' = Map.insertWith (flip const) c d m
                                  in if Map.size m' == sz then m'
                                     else scan sz m' s (n + 1)

pwd' s = scan 8 Map.empty s 0 & Map.toList & map snd & map toChar

day5b ls = parse ls & pwd'
