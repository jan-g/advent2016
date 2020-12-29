import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)
import Data.List as L
import Data.Maybe (catMaybes)

import Lib
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


main :: IO ()
main =
  hspec $ do
    describe "Day1" $ do
      it "correctly runs on the test data" $ do
        Day1.dist (Day1.run Day1.start $ Day1.parse ["R2, L3"]) `shouldBe` 5
        (Day1.run Day1.start $ Day1.parse ["R2, R2, R2"]) `shouldBe` ((0,-2), (-1,0))
        Day1.dist (Day1.run Day1.start $ Day1.parse ["R5, L5, R5, R3"]) `shouldBe` 12

      it "solves part 2" $ do
        let (pos, dir) = Day1.trace (Day1.parse ["R8, R4, R4, R8"])
        pos `shouldBe` (4, 0)

    describe "Day 2" $ do
      let example = "ULL\n\
                    \RRDDD\n\
                    \LURDL\n\
                    \UUUUD" & lines
      it "follows a series of coordinates" $ do
        Day2.followAll example `shouldBe` ([(-1,1), (1,-1), (0,-1), (0, 0)], (0, 0))
        Day2.decode example `shouldBe` [1,9,8,5]