module Day4 where

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
import qualified Data.Counter as C

import Lib


{-
--- Day 4: Security Through Obscurity ---

Finally, you come across an information kiosk with a list of rooms. Of course, the list is encrypted and full of decoy data, but the instructions to decode the list are barely hidden nearby. Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with ties broken by alphabetization. For example:

    aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
    a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
    not-a-real-room-404[oarel] is a real room.
    totally-real-room-200[decoy] is not.

Of the real rooms from the list above, the sum of their sector IDs is 1514.

What is the sum of the sector IDs of the real rooms?
-}

data Room = Room [String] Int String deriving (Show, Eq)

parse ls = ls
         & map (quickParse parseRoom)
         & catMaybes

parseRoom :: ReadP Room
parseRoom = do
  strs <- P.sepBy (many1 (satisfy isAlpha)) (char '-')
  char '-'
  id <- natParser
  char '['
  check <- wordParser
  char ']'
  eof
  return $ Room strs (fromIntegral id) check


valid (Room s _ chk) =
  let cs = C.count (concat s)
         & Map.toList
         & sortOn (\(k, v) -> (-v, k))
         & map fst
         & take 5
  in cs == chk


idOf (Room _ i _) = i


day4 ls = parse ls
        & filter valid
        & map idOf
        & sum

{-
--- Part Two ---

With all the decoy data out of the way, it's time to decrypt this list and get moving.

The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.

To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.

What is the sector ID of the room where North Pole objects are stored?
-}

rotN n '-' = ' '
rotN n x | isAlpha x = chr $ (ord 'a') + (ord x - ord 'a' + n) `mod` 26

decrypt (Room s n _) = map (map (rotN n)) s & intercalate [' ']

day4b ls = parse ls & filter ((=="northpole object storage") . decrypt)
