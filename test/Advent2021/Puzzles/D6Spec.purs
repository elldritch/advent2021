module Advent2021.Puzzles.D6Spec (spec) where

import Prelude

import Advent2021.Puzzles.D6 as D6
import Advent2021.Spec.Assertions (fromJust, shouldSucceed)
import Data.BigInt as BigInt
import Test.Spec (Spec, describe, it)

input :: String
input = "3,4,3,1,2\n"

spec :: Spec Unit
spec =
  describe "Day 6" do
    it "simulates lanternfish count after 80 days" do
      D6.part1 input `shouldSucceed` BigInt.fromInt 5934
    it "simulates lanternfish count after 256 days" do
      x <- fromJust $ BigInt.fromString "26984457539"
      D6.part2 input `shouldSucceed` x
