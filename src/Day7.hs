module Day7 where

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
import Debug.Trace (trace)

import Lib


{-
--- Day 7: Internet Protocol Version 7 ---

While snooping around the local network of EBHQ, you compile a list of IP addresses (they're IPv7, of course; IPv6 is much too limited). You'd like to figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA. An ABBA is any four-character sequence which consists of a pair of two different characters followed by the reverse of that pair, such as xyyx or abba. However, the IP also must not have an ABBA within any hypernet sequences, which are contained by square brackets.

For example:

    abba[mnop]qrst supports TLS (abba outside square brackets).
    abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
    aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters must be different).
    ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though it's within a larger string).

How many IPs in your puzzle input support TLS?

To begin, get your puzzle input.
-}

parse ls = map parseLine ls & catMaybes

parseLine x = let a = quickParse adr x
                  b = quickParse brk x
              in
              if isJust a && isJust b then Just (fromJust a, fromJust b) else Nothing

alphaParser = many1 (satisfy isAlpha)

adr :: ReadP [String]
adr = P.sepBy alphaParser (char '[' *> alphaParser <* char ']') <* eof

brk :: ReadP [String]
brk = alphaParser >>>> (P.sepBy (between (char '[') (char ']') alphaParser) alphaParser) <<<< alphaParser <<<< eof

cromulent (a:b:c:d:rs)
  | a /= b && a == d && b == c = True
  | otherwise                  = cromulent $ b:c:d:rs
cromulent _ = False

passable (as, bs) = any cromulent as && not (any cromulent bs) 

day7 ls = parse ls & filter passable & length

{-
-}

day7b ls = "hello world"
