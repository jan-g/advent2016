import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)
import Data.List as L
import Data.Maybe (catMaybes, fromJust)
import qualified Data.ByteString.UTF8 as BSU

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
    describe "Day 7" $ do
      it "works out cromulent strings" $ do
        map Day7.cromulent ["abba", "mnop", "qrst", "aaaa", "koxxol"] `shouldBe` [True, False, False, False, True]

      it "applies the rules" $ do
        let example = "abba[mnop]qrst\n\
                      \abcd[bddb]xyyx\n\
                      \aaaa[qwer]tyui\n\
                      \ioxxoj[asdfgh]zxcvbn" & lines
            egs = Day7.parse example
        map Day7.passable egs `shouldBe` [True, False, False, True]

      it "locates ABAs" $ do
        Day7.findABAs "zazbz" `shouldBe` Set.fromList [('z', 'a'), ('z', 'b')]

      it "confirms SSL operation" $ do
        let example = "aba[bab]xyz\n\
                      \xyx[xyx]xyx\n\
                      \aaa[kek]eke\n\
                      \zazbz[bzb]cdb" & lines
            egs = Day7.parse example
        map Day7.doesSsl egs `shouldBe` [True, False, True, True]

    describe "day 9" $ do
      forM_ [ ("advent", "advent")
            , ("A(1x5)BC", "ABBBBBC")
            , ("(3x3)XYZ", "XYZXYZXYZ")
            , ("A(2x2)BCD(2x2)EFG", "ABCBCDEFEFG")
            , ("(6x1)(1x3)A", "(1x3)A")
            , ("X(8x2)(3x3)ABCY", "X(3x3)ABC(3x3)ABCY")
            ] $ \(i, o) -> do
        it ("should expand " ++ i) $ do
          quickParse Day9.expand i `shouldBe` Just o

      forM_ [ ("(3x3)XYZ", fromIntegral $ length "XYZXYZXYZ")
            , ("X(8x2)(3x3)ABCY", fromIntegral $ length "XABCABCABCABCABCABCY")
            , ("(27x12)(20x12)(13x14)(7x10)(1x12)A", 241920)
            , ("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 445)
            ] $ \(i, o) -> do
        it ("should expand " ++ i) $ do
          quickParse Day9.expand' i `shouldBe` Just o
