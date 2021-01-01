module Day14 where

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

import Debug.Trace (trace)

import Lib


{-
--- Day 14: One-Time Pad ---

In order to communicate securely with Santa while you're on this mission, you've been using a one-time pad that you generate using a pre-agreed algorithm. Unfortunately, you've run out of keys in your one-time pad, and so you need to generate some more.

To generate keys, you first get a stream of random data by taking the MD5 of a pre-arranged salt (your puzzle input) and an increasing integer index (starting with 0, and represented in decimal); the resulting MD5 hash should be represented as a string of lowercase hexadecimal digits.

However, not all of these MD5 hashes are keys, and you need 64 new keys for your one-time pad. A hash is a key only if:

    It contains three of the same character in a row, like 777. Only consider the first such triplet in a hash.
    One of the next 1000 hashes in the stream contains that same character five times in a row, like 77777.

Considering future hashes for five-of-a-kind sequences does not cause those hashes to be skipped; instead, regardless of whether the current hash is a key, always resume testing for keys starting with the very next hash.

For example, if the pre-arranged salt is abc:

    The first index which produces a triple is 18, because the MD5 hash of abc18 contains ...cc38887a5.... However, index 18 does not count as a key for your one-time pad, because none of the next thousand hashes (index 19 through index 1018) contain 88888.
    The next index which produces a triple is 39; the hash of abc39 contains eee. It is also the first key: one of the next thousand hashes (the one at index 816) contains eeeee.
    None of the next six triples are keys, but the one after that, at index 92, is: it contains 999 and index 200 contains 99999.
    Eventually, index 22728 meets all of the criteria to generate the 64th key.

So, using our example salt of abc, index 22728 produces the 64th key.

Given the actual salt in your puzzle input, what index produces your 64th one-time pad key?
-}

parse ls = ls
         & head

hashStream salt = map (\i -> (i, hash (salt ++ show i))) [0..]

hash str = -- trace (show idx) $
         let cs@[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] = MD5.hash (BSU.fromString str) & B.unpack
         in -- trace ("hash is " ++ show cs) $
            map toChar [ a `div` 16, a `mod` 16,
                         b `div` 16, b `mod` 16,
                         c `div` 16, c `mod` 16,
                         d `div` 16, d `mod` 16,
                         e `div` 16, e `mod` 16,
                         f `div` 16, f `mod` 16,
                         g `div` 16, g `mod` 16,
                         h `div` 16, h `mod` 16,
                         i `div` 16, i `mod` 16,
                         j `div` 16, j `mod` 16,
                         k `div` 16, k `mod` 16,
                         l `div` 16, l `mod` 16,
                         m `div` 16, m `mod` 16,
                         n `div` 16, n `mod` 16,
                         o `div` 16, o `mod` 16,
                         p `div` 16, p `mod` 16 ]

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

triple (a:b:c:rs)
  | a == b && b == c = Just a
  | otherwise        = triple (b:c:rs)
triple _ = Nothing

quintuple v (a:b:c:d:e:rs) = a == v && b == v && c == v && d == v && e == v || quintuple v (b:c:d:e:rs)
quintuple _ _ = False

-- we return indexes so that we can find the index generating the nth item
digits ((idx, h):hs) =
  case triple h of
    Just d -> if any (quintuple d) (map snd $ take 1000 hs) then (idx, d):digits hs else digits hs
    Nothing -> digits hs


day14 ls =
  let salt = parse ls
      hs = hashStream salt
      ds = digits hs
  in ds !! 63 & fst

{-
--- Part Two ---

Of course, in order to make this process even more secure, you've also implemented key stretching.

Key stretching forces attackers to spend more time generating hashes. Unfortunately, it forces everyone else to spend more time, too.

To implement key stretching, whenever you generate a hash, before you use it, you first find the MD5 hash of that hash, then the MD5 hash of that hash, and so on, a total of 2016 additional hashings. Always use lowercase hexadecimal representations of hashes.

For example, to find the stretched hash for index 0 and salt abc:

    Find the MD5 hash of abc0: 577571be4de9dcce85a041ba0410f29f.
    Then, find the MD5 hash of that hash: eec80a0c92dc8a0777c619d9bb51e910.
    Then, find the MD5 hash of that hash: 16062ce768787384c81fe17a7a60c7e3.
    ...repeat many times...
    Then, find the MD5 hash of that hash: a107ff634856bb300138cac6568c0f24.

So, the stretched hash for index 0 in this situation is a107ff.... In the end, you find the original hash (one use of MD5), then find the hash-of-the-previous-hash 2016 times, for a total of 2017 uses of MD5.

The rest of the process remains the same, but now the keys are entirely different. Again for salt abc:

    The first triple (222, at index 5) has no matching 22222 in the next thousand hashes.
    The second triple (eee, at index 10) hash a matching eeeee at index 89, and so it is the first key.
    Eventually, index 22551 produces the 64th key (triple fff with matching fffff at index 22859.

Given the actual salt in your puzzle input and using 2016 extra MD5 calls of key stretching, what index now produces your 64th one-time pad key?
-}

stretch inp = (iterate hash inp) !! 2017

stretchStream salt = map (\i -> (i, stretch (salt ++ show i))) [0..]

day14b ls =
  let salt = parse ls
      hs = stretchStream salt
      ds = digits hs
  in ds !! 63 & fst
