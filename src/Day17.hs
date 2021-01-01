module Day17 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Bits ((.&.), (.|.))
import MD5 (hash)

import Debug.Trace (trace)

import Lib


{-
--- Day 17: Two Steps Forward ---

You're trying to access a secure vault protected by a 4x4 grid of small rooms connected by doors. You start in the top-left room (marked S), and you can access the vault (marked V) once you reach the bottom-right room:

#########
#S| | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | |
####### V

Fixed walls are marked with #, and doors are marked with - or |.

The doors in your current room are either open or closed (and locked) based on the hexadecimal MD5 hash of a passcode (your puzzle input) followed by a sequence of uppercase characters representing the path you have taken so far (U for up, D for down, L for left, and R for right).

Only the first four characters of the hash are used; they represent, respectively, the doors up, down, left, and right from your current position. Any b, c, d, e, or f means that the corresponding door is open; any other character (any number or a) means that the corresponding door is closed and locked.

To access the vault, all you need to do is reach the bottom-right room; reaching this room opens the vault and all doors in the maze.

For example, suppose the passcode is hijkl. Initially, you have taken no steps, and so your path is empty: you simply find the MD5 hash of hijkl alone. The first four characters of this hash are ced9, which indicate that up is open (c), down is open (e), left is open (d), and right is closed and locked (9). Because you start in the top-left corner, there are no "up" or "left" doors to be open, so your only choice is down.

Next, having gone only one step (down, or D), you find the hash of hijklD. This produces f2bc, which indicates that you can go back up, left (but that's a wall), or right. Going right means hashing hijklDR to get 5745 - all doors closed and locked. However, going up instead is worthwhile: even though it returns you to the room you started in, your path would then be DU, opening a different set of doors.

After going DU (and then hashing hijklDU to get 528e), only the right door is open; after going DUR, all doors lock. (Fortunately, your actual passcode is not hijkl).

Passcodes actually used by Easter Bunny Vault Security do allow access to the vault if you know the right path. For example:

    If your passcode were ihgpwlah, the shortest path would be DDRRRD.
    With kglvqrro, the shortest path would be DDUDRLRRUDRD.
    With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.

Given your vault's passcode, what is the shortest path (the actual path, not just the length) to reach the vault?


Your puzzle input is yjjvjgan.
-}

parse ls = head ls

exits (x, y, path) =
  let (up:down:left:right:_) = hash path
  in  [ if y > 0 && open up then Just (x, y-1, path ++ "U") else Nothing
      , if y < 3 && open down then Just (x, y+1, path ++ "D") else Nothing
      , if x > 0 && open left then Just (x-1, y, path ++ "L") else Nothing
      , if x < 3 && open right then Just (x+1, y, path ++ "R") else Nothing
      ] & catMaybes

open 'b' = True
open 'c' = True
open 'd' = True
open 'e' = True
open 'f' = True
open _ = False

next (c, (x, y, path)) = if (x,y) == (3,3) then Set.empty
                          else Set.fromList [(succ c, p) | p <- exits (x, y, path)]

finished (c, (x, y, path)) = x == 3 && y == 3

hunt prefix =
  bfs next finished id (0, (0, 0, prefix))

day17 ls = let prefix = parse ls
               Just (_, (_, _, path)) = hunt prefix
           in  drop (length prefix) path

{-
--- Part Two ---

You're curious how robust this security solution really is, and so you decide to find longer and longer paths which still provide access to the vault. You remember that paths always end the first time they reach the bottom-right room (that is, they can never pass through it, only end in it).

For example:

    If your passcode were ihgpwlah, the longest path would take 370 steps.
    With kglvqrro, the longest path would be 492 steps long.
    With ulqzkmiv, the longest path would be 830 steps long.

What is the length of the longest path that reaches the vault?
-}


dfs :: (state -> Set.Set state)
    -> (state -> Bool)
    -> (state -> state -> state)
    -> state
    -> Maybe state
dfs next finished better state =
  if finished state then Just state
  else
    foldl (\best state' -> case dfs next finished better state' of
                            Just performance ->
                               case best of Nothing -> Just performance
                                            Just oldBest -> Just (better oldBest performance)
                            Nothing -> best) Nothing (next state)

longest prefix =
  fst <$> dfs next finished max (0, (0, 0, prefix))

day17b ls = longest (parse ls)
