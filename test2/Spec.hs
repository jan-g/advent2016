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

    describe "day 10" $ do
      let example = "value 5 goes to bot 2\n\
                    \bot 2 gives low to bot 1 and high to bot 0\n\
                    \value 3 goes to bot 1\n\
                    \bot 1 gives low to output 1 and high to bot 0\n\
                    \bot 0 gives low to output 2 and high to output 0\n\
                    \value 2 goes to bot 2" & lines
      it "runs the example" $ do
        let s = Day10.parse example & Day10.makeState & Day10.run
        Day10.outputs s `shouldBe` Map.fromList [(0, 5), (1, 2), (2, 3)]
        Day10.comparisons s Map.! (2, 5) `shouldBe` 2

    describe "Day 11" $ do
      it "Parses" $ do
        quickParse Day11.item "a frobnitz generator" `shouldBe` Just (Day11.Gen "frobnitz")
        quickParse Day11.item "a blank-compatible microchip" `shouldBe` Just (Day11.Chip "blank")
        quickParse Day11.items "a baz generator, and a foo-compatible microchip" `shouldBe` Just (Set.fromList [Day11.Gen "baz", Day11.Chip "foo"])
        quickParse Day11.items "a baz generator and a foo-compatible microchip" `shouldBe` Just (Set.fromList [Day11.Gen "baz", Day11.Chip "foo"])

      let example = "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
                    \The second floor contains a hydrogen generator.\n\
                    \The third floor contains a lithium generator.\n\
                    \The fourth floor contains nothing relevant." & lines
          layout = Day11.parse example
      it "parses the example" $ do
        layout `shouldBe` Map.fromList [(1, Set.fromList [Day11.Chip "hydrogen", Day11.Chip "lithium"]),
                                        (2, Set.fromList [Day11.Gen "hydrogen"]),
                                        (3, Set.fromList [Day11.Gen "lithium"]),
                                        (4, Set.fromList [])]

      it "generates sets of one or two items" $ do
        Day11.oneOrTwoItems (Set.fromList [1,2,3,4]) `shouldBe` Set.fromList [Set.singleton 1, Set.singleton 2, Set.singleton 3, Set.singleton 4,
                                                                              Set.fromList [1,2], Set.fromList [1,3], Set.fromList [1,4],
                                                                              Set.fromList [2,3], Set.fromList [2,4],
                                                                              Set.fromList [3,4]]

      forM_ [([], True),
             ([Day11.Chip "a", Day11.Chip "b"], True),
             ([Day11.Chip "a", Day11.Gen "b"], False),
             ([Day11.Chip "a", Day11.Gen "a"], True),
             ([Day11.Chip "a", Day11.Gen "a", Day11.Chip "b"], False),
             ([Day11.Chip "a", Day11.Gen "a", Day11.Gen "b"], True)
             ] $ \(i, o) -> do
        it ("checks validity of " ++ show i) $ do
          Day11.valid (Set.fromList i) `shouldBe` o

      it "seaches the example" $ do
        (Day11.day11 example & fromJust & fst) `shouldBe` 11

      let broken = "The first floor contains a hydrogen-compatible microchip.\n\
                   \The second floor contains nothing relevant.\n\
                   \The third floor contains a lithium generator.\n\
                   \The fourth floor contains nothing relevant." & lines
      it "correctly surmises that a broken configuraiton has no solution" $ do
        Day11.day11 broken `shouldBe` Nothing

    describe "Day 12" $ do
      let example = "cpy 41 a\n\
                    \inc a\n\
                    \inc a\n\
                    \dec a\n\
                    \jnz a 2\n\
                    \dec a" & lines
      it "parses" $ do
        Map.size (Day12.parse example) `shouldBe` 6
      it "runs the example" $ do
        let rs = Day12.run (Day12.parse example) 0 Map.empty
        rs `shouldBe` Map.fromList [('a', 42)]

    describe "Day 13" $ do
      it "searches the example problem" $ do
        Day13.hunt 10 (7, 4) `shouldBe` Just 11

    describe "day 14" $ do
      it "generates a hash stream" $ do
        let hs = take 1000 $ Day14.hashStream "abc"
        Day14.triple (hs !! 18 & snd) `shouldBe` Just '8'
        Day14.quintuple 'e' (hs !! 816 & snd) `shouldBe` True
      it "hunts a hash stream" $ do
        let hs = Day14.hashStream "abc"
            ds = Day14.digits hs
        take 2 ds `shouldBe` [(39, 'e'), (92, '9')]
        fst (ds !! 63) `shouldBe` 22728 
      
      it "stretches hashes" $ do
        Day14.stretch "abc0" `shouldBe` "a107ff634856bb300138cac6568c0f24"

      it "hunts a stretched stream" $ do
        let hs = take 1000 $ Day14.stretchStream "abc"
            ds = Day14.digits hs
        head ds `shouldBe` (10, 'e')
--        fst (ds !! 63) `shouldBe` 22551
