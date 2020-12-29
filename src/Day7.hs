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
--- Part Two ---

You would also like to know which IPs support SSL (super-secret listening).

An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the supernet sequences (outside any square bracketed sections), and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences. An ABA is any three-character sequence which consists of the same character twice with a different character between them, such as xyx or aba. A corresponding BAB is the same characters but in reversed positions: yxy and bab, respectively.

For example:

    aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab within square brackets).
    xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
    aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet; the aaa sequence is not related, because the interior character must be different).
    zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a corresponding bzb, even though zaz and zbz overlap).

How many IPs in your puzzle input support SSL?
-}

aba (a:b:c:_) = if a /= b && a == c then Set.singleton (a, b) else Set.empty
aba _ = Set.empty

findABAs s = tails s & map aba & Set.unions

swap (a, b) = (b, a)

doesSsl (as, bs) = let abas = map findABAs as & Set.unions
                       babs = map findABAs bs & Set.unions & Set.map swap
                   in not . Set.null $ Set.intersection abas babs

day7b ls = parse ls & filter doesSsl & length
