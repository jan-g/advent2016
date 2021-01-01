{-# LANGUAGE LambdaCase #-}

module Day11 where

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
--- Day 11: Radioisotope Thermoelectric Generators ---

You come upon a column of four floors that have been entirely sealed off from the rest of the building except for a small dedicated lobby. There are some radiation warnings and a big sign which reads "Radioisotope Testing Facility".

According to the project status board, this facility is currently being used to experiment with Radioisotope Thermoelectric Generators (RTGs, or simply "generators") that are designed to be paired with specially-constructed microchips. Basically, an RTG is a highly radioactive rock that generates electricity through heat.

The experimental RTGs have poor radiation containment, so they're dangerously radioactive. The chips are prototypes and don't have normal radiation shielding, but they do have the ability to generate an electromagnetic radiation shield when powered. Unfortunately, they can only be powered by their corresponding RTG. An RTG powering a microchip is still dangerous to other microchips.

In other words, if a chip is ever left in the same area as another RTG, and it's not connected to its own RTG, the chip will be fried. Therefore, it is assumed that you will follow procedure and keep chips connected to their corresponding RTG when they're in the same room, and away from other RTGs otherwise.

These microchips sound very interesting and useful to your current activities, and you'd like to try to retrieve them. The fourth floor of the facility has an assembling machine which can make a self-contained, shielded computer for you to take with you - that is, if you can bring it all of the RTGs and microchips.

Within the radiation-shielded part of the facility (in which it's safe to have these pre-assembly RTGs), there is an elevator that can move between the four floors. Its capacity rating means it can carry at most yourself and two RTGs or microchips in any combination. (They're rigged to some heavy diagnostic equipment - the assembling machine will detach it for you.) As a security measure, the elevator will only function if it contains at least one RTG or microchip. The elevator always stops on each floor to recharge, and this takes long enough that the items within it and the items on that floor can irradiate each other. (You can prevent this if a Microchip and its Generator end up on the same floor in this way, as they can be connected while the elevator is recharging.)

You make some notes of the locations of each component of interest (your puzzle input). Before you don a hazmat suit and start moving things around, you'd like to have an idea of what you need to do.

When you enter the containment area, you and the elevator will start on the first floor.

For example, suppose the isolated area has the following arrangement:

The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.

As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium, M for Microchip, and G for Generator), the initial state looks like this:

F4 .  .  .  .  .
F3 .  .  .  LG .
F2 .  HG .  .  .
F1 E  .  HM .  LM

Then, to get everything up to the assembling machine on the fourth floor, the following steps could be taken:

    Bring the Hydrogen-compatible Microchip to the second floor, which is safe because it can get power from the Hydrogen Generator:

    F4 .  .  .  .  .
    F3 .  .  .  LG .
    F2 E  HG HM .  .
    F1 .  .  .  .  LM

    Bring both Hydrogen-related items to the third floor, which is safe because the Hydrogen-compatible microchip is getting power from its generator:

    F4 .  .  .  .  .
    F3 E  HG HM LG .
    F2 .  .  .  .  .
    F1 .  .  .  .  LM

    Leave the Hydrogen Generator on floor three, but bring the Hydrogen-compatible Microchip back down with you so you can still use the elevator:

    F4 .  .  .  .  .
    F3 .  HG .  LG .
    F2 E  .  HM .  .
    F1 .  .  .  .  LM

    At the first floor, grab the Lithium-compatible Microchip, which is safe because Microchips don't affect each other:

    F4 .  .  .  .  .
    F3 .  HG .  LG .
    F2 .  .  .  .  .
    F1 E  .  HM .  LM

    Bring both Microchips up one floor, where there is nothing to fry them:

    F4 .  .  .  .  .
    F3 .  HG .  LG .
    F2 E  .  HM .  LM
    F1 .  .  .  .  .

    Bring both Microchips up again to floor three, where they can be temporarily connected to their corresponding generators while the elevator recharges, preventing either of them from being fried:

    F4 .  .  .  .  .
    F3 E  HG HM LG LM
    F2 .  .  .  .  .
    F1 .  .  .  .  .

    Bring both Microchips to the fourth floor:

    F4 E  .  HM .  LM
    F3 .  HG .  LG .
    F2 .  .  .  .  .
    F1 .  .  .  .  .

    Leave the Lithium-compatible microchip on the fourth floor, but bring the Hydrogen-compatible one so you can still use the elevator; this is safe because although the Lithium Generator is on the destination floor, you can connect Hydrogen-compatible microchip to the Hydrogen Generator there:

    F4 .  .  .  .  LM
    F3 E  HG HM LG .
    F2 .  .  .  .  .
    F1 .  .  .  .  .

    Bring both Generators up to the fourth floor, which is safe because you can connect the Lithium-compatible Microchip to the Lithium Generator upon arrival:

    F4 E  HG .  LG LM
    F3 .  .  HM .  .
    F2 .  .  .  .  .
    F1 .  .  .  .  .

    Bring the Lithium Microchip with you to the third floor so you can use the elevator:

    F4 .  HG .  LG .
    F3 E  .  HM .  LM
    F2 .  .  .  .  .
    F1 .  .  .  .  .

    Bring both Microchips to the fourth floor:

    F4 E  HG HM LG LM
    F3 .  .  .  .  .
    F2 .  .  .  .  .
    F1 .  .  .  .  .

In this arrangement, it takes 11 steps to collect all of the objects at the fourth floor for assembly. (Each elevator stop counts as one step, even if nothing is added to or removed from it.)

In your situation, what is the minimum number of steps required to bring all of the objects to the fourth floor?

To begin, get your puzzle input.
-}

data Item = Gen String | Chip String deriving (Show, Eq, Ord)

parse :: [String] -> Map.Map Integer (Set.Set Item)
parse ls = ls
         & map (quickParse contents)
         & catMaybes
         & Map.fromList

contents = do
  string "The "
  f <- ordinal
  string " floor contains "
  is <- items
  string "."
  eof
  return (f, is)

ordinal = (string "first" *> pure 1) <++
          (string "second" *> pure 2) <++
          (string "third" *> pure 3) <++
          (string "fourth" *> pure 4)

items = (string "nothing relevant" *> pure Set.empty) <++
  (do
    is <- P.sepBy item (string ", ")
    optional (char ',')
    string " and "
    last <- item
    return $ Set.fromList (last:is)) <++
  (pure Set.singleton <*> item)

item = (string "a " *> (pure Gen <*> munch1 isAlpha) <* string " generator") <++
       (string "a " *> (pure Chip <*> munch1 isAlpha) <* string "-compatible microchip")


data State = State { elevatorAt :: Integer
                   , floors :: Map.Map Integer (Set.Set Item)
                   }
  deriving (Show, Eq, Ord)

valid :: Set.Set Item -> Bool
valid is = let rtg = Set.filter (\case Gen _ -> True; _ -> False) is
           in
           all (\i -> case i of
                        Chip x -> Set.null rtg || Set.member (Gen x) is 
                        Gen _ -> True) is

finished :: (Integer, State) -> Bool
finished (_, s) = all (\(f, is) -> f == 4 || Set.null is) (floors s & Map.toList)

oneItem is = Set.map (Set.singleton) is
oneOrTwoItems is = let ones = oneItem is & Set.toList
                   in  Set.fromList [i `Set.union` j | i <- ones, j <- ones]

next :: (Integer, State) -> Set.Set (Integer, State)
next (c, s) =
  let f = elevatorAt s
      items = (floors s) Map.! f
      movables = oneOrTwoItems items
      canTake = Set.filter (\is -> Set.difference items is & valid) movables
      movesUp = if f == 4 then Set.empty
                else
                  let f' = f + 1
                  in Set.map (\is -> floors s
                                   & Map.insert f (items `Set.difference` is)
                                   & Map.insertWith Set.union f' is) canTake
                   & Set.filter (\fs -> fs Map.! f' & valid)
                   & Set.map (\fs -> s { elevatorAt=f', floors=fs })
      movesDn = if f == 1
                   || f == 2 && Set.null (floors s Map.! 1)
                   || f == 3 && Set.null (floors s Map.! 1) && Set.null (floors s Map.! 2)
                   then Set.empty
                else
                  let f' = f - 1
                  in Set.map (\is -> floors s
                                   & Map.insert f (items `Set.difference` is)
                                   & Map.insertWith Set.union f' is) canTake
                   & Set.filter (\fs -> fs Map.! f' & valid)
                   & Set.map (\fs -> s { elevatorAt=f', floors=fs })
  in Set.map (\m -> (c+1, m)) $ movesUp `Set.union` movesDn
  
summariseState :: State -> (Integer, [(Integer, Integer)])
summariseState (State { elevatorAt=elev, floors=fls }) =
  let (gens, chips) = Map.foldlWithKey
                       (\(gens, chips) fl items ->
                         Set.foldl (\(gens, chips) i -> case i of
                                                          Gen h -> (Map.insert h fl gens, chips)
                                                          Chip h -> (gens, Map.insert h fl chips))
                         (gens, chips) items) (Map.empty, Map.empty) fls
      g' = Map.toAscList gens & map snd
      c' = Map.toAscList chips & map snd
  in (elev, sort (zip g' c'))

initialState fs = State { elevatorAt=1, floors=fs }  --  previous=Set.empty

day11 ls =
  let s0 = parse ls & initialState
  in  bfs next finished summariseState (0, s0)


{-
--- Part Two ---

You step into the cleanroom separating the lobby from the isolated area and put on the hazmat suit.

Upon entering the isolated containment area, however, you notice some extra parts on the first floor that weren't listed on the record outside:

    An elerium generator.
    An elerium-compatible microchip.
    A dilithium generator.
    A dilithium-compatible microchip.

These work just like the other generators and microchips. You'll have to get them up to assembly as well.

What is the minimum number of steps required to bring all of the objects, including these four new ones, to the fourth floor?
-}

day11b ls =
  let s0 = parse ls
         & Map.insertWith Set.union 1 (Set.singleton $ Gen "elerium")
         & Map.insertWith Set.union 1 (Set.singleton $ Chip "elerium")
         & Map.insertWith Set.union 1 (Set.singleton $ Gen "dilithium")
         & Map.insertWith Set.union 1 (Set.singleton $ Chip "dilithium")
         & initialState
  in  bfs next finished summariseState (0, s0)
