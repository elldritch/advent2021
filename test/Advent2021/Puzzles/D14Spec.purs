module Advent2021.Puzzles.D14Spec (spec) where

import Prelude
import Advent2021.Puzzles.D14 as D14
import Advent2021.Spec.Assertions (fromJust, shouldSucceed)
import Data.BigInt as BigInt
import Test.Spec (Spec, describe, it)

input :: String
input =
  """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"""

spec :: Spec Unit
spec =
  describe "Day 14" do
    it "counts elements after 10 steps of polymerization" do
      D14.part1 input `shouldSucceed` BigInt.fromInt 1588
    it "counts elements after 40 steps of polymerization" do
      x <- fromJust $ BigInt.fromString "2188189693529"
      D14.part2 input `shouldSucceed` x
