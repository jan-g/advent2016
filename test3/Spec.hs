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
import qualified Text.ParserCombinators.ReadP as P

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

    describe "day 20" $ do
      let example = "5-8\n\
                    \0-2\n\
                    \4-7" & lines
      it "runs the trivial example" $ do
        Day20.day20 example `shouldBe` Just 3
      it "counts the available IP addresses" $ do
        let ranges = Day20.parse example
            q = Day20.rangesToHeap ranges
        Day20.process' 10 0 0 0 q `shouldBe` 2
        Day20.process' 9 0 0 0 q `shouldBe` 1

    describe "day 21" $ do
      it "handles individual moves" $ do
        -- swap position 4 with position 0 swaps the first and last letters, producing the input for the next step, ebcda.
        Day21.interpret (Day21.SwapPos 4 0) "abcde" `shouldBe` "ebcda"
        -- swap letter d with letter b swaps the positions of d and b: edcba.
        Day21.interpret (Day21.SwapChar 'd' 'b') "ebcda" `shouldBe` "edcba"
        -- reverse positions 0 through 4 causes the entire string to be reversed, producing abcde.
        Day21.interpret (Day21.Reverse 0 4) "edcba" `shouldBe` "abcde"
        -- rotate left 1 step shifts all letters left one position, causing the first letter to wrap to the end of the string: bcdea.
        Day21.interpret (Day21.RotRight (-1)) "abcde" `shouldBe` "bcdea"
        -- move position 1 to position 4 removes the letter at position 1 (c), then inserts it at position 4 (the end of the string): bdeac.
        Day21.interpret (Day21.Move 1 4) "bcdea" `shouldBe` "bdeac"
        -- move position 3 to position 0 removes the letter at position 3 (a), then inserts it at position 0 (the front of the string): abdec.
        Day21.interpret (Day21.Move 3 0) "bdeac" `shouldBe` "abdec"
        -- rotate based on position of letter b finds the index of letter b (1), then rotates the string right once plus a number of times equal to that index (2): ecabd.
        Day21.interpret (Day21.RotOnChar 'b') "abdec" `shouldBe` "ecabd"
        -- rotate based on position of letter d finds the index of letter d (4), then rotates the string right once, plus a number of times equal to that index, plus an additional time because the index was at least 4, for a total of 6 right rotations: decab.
        Day21.interpret (Day21.RotOnChar 'd') "ecabd" `shouldBe` "decab"

      it "runs the example" $ do
        let example = "swap position 4 with position 0\n\
                      \swap letter d with letter b\n\
                      \reverse positions 0 through 4\n\
                      \rotate left 1 step\n\
                      \move position 1 to position 4\n\
                      \move position 3 to position 0\n\
                      \rotate based on position of letter b\n\
                      \rotate based on position of letter d" & lines
            prog = Day21.parse example
        length prog `shouldBe` 8
        Day21.process prog "abcde" `shouldBe` "decab"

    describe "day 22" $ do
      it "parses an example line" $ do
        quickParse Day22.node
           "/dev/grid/node-x28-y31   92T   72T    20T   78%"
           `shouldBe` Just ((28,31),(92,72))

    describe "day 23" $ do
      let example = "cpy 2 a\n\
                    \tgl a\n\
                    \tgl a\n\
                    \tgl a\n\
                    \cpy 1 a\n\
                    \dec a\n\
                    \dec a" & lines
          prog = Day23.parse example
      it "Runs the sample prog" $ do
        Day23.run prog 0 Map.empty `shouldBe` Map.singleton 'a' 3