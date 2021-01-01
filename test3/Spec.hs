import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)
import Data.List as L
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Sequence as Seq

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
    describe "Day 15" $ do
      let example = "Disc #1 has 5 positions; at time=0, it is at position 4.\n\
                    \Disc #2 has 2 positions; at time=0, it is at position 1." & lines
      it "parses" $ do
        Day15.parse example `shouldBe` [(1, 5, 4), (2, 2, 1)]

      it "solves" $ do
        Day15.day15 example `shouldBe` 5

    describe "Day 16" $ do
      forM_ [("1", "100"),
             ("0", "001"),
             ("11111", "11111000000"),
             ("111100001010", "1111000010100101011110000")
             ] $ \(i, o) -> do
        it ("expands " ++ i) $ do
          Day16.expand i `shouldBe` o

      it "checksums correctly" $ do
        Day16.repeatedChecksum "110010110100" `shouldBe` "100"

      it "solves the example" $ do
        Day16.partA "10000" 20 `shouldBe` "01100"

    describe "day 17" $ do
      it "correctly summarises failure" $ do
        Day17.hunt "hijkl" `shouldBe` Nothing
      it "solves ex1" $ do
        Day17.hunt "ihgpwlah" `shouldBe` Just (6, (3, 3, "ihgpwlahDDRRRD"))
      it "solves ex2" $ do
        snd <$> (Day17.hunt "kglvqrro") `shouldBe` Just (3, 3, "kglvqrroDDUDRLRRUDRD")
      it "solves ex3" $ do
        snd <$> Day17.hunt "ulqzkmiv" `shouldBe` Just (3, 3, "ulqzkmivDRURDRUDDLLDLUURRDULRLDUUDDDRR")

      forM_ [ ("ihgpwlah", 370)
            , ("kglvqrro", 492)
            , ("ulqzkmiv", 830)
            ] $ \(i, o) -> do
        it ("finds the longest for " ++ i) $ do
          Day17.longest i `shouldBe` Just o

    describe "day 18" $ do
      it "computes a new row" $ do
        Day18.next "..^^." `shouldBe` ".^^^^"
      
      it "works out the larget example" $ do
        (iterate Day18.next ".^^.^.^^^^" & take 10 & concat & filter (=='.') & length) `shouldBe` 38

    describe "Day 19" $ do
      it "runs the small example" $ do
        let elves = Day19.starting 5
        Day19.move 1 elves `shouldBe` Map.singleton 3 (3, 5)
      
      it ("runs part2 for 5") $ do
        (Day19.part2 $ Seq.fromList [1..5]) `shouldBe` 2

      it "runs part 2" $ do
        Day19.day19b ["5"] `shouldBe` Map.singleton 2 2