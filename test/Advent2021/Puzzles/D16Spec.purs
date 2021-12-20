module Advent2021.Puzzles.D16Spec
  ( spec
  ) where

import Prelude
import Advent2021.Puzzles.D16 as D16
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input1 :: String
input1 = "D2FE28"

input2 :: String
input2 = "38006F45291200"

input3 :: String
input3 = "EE00D40C823060"

input4 :: String
input4 = "8A004A801A8002F478"

input5 :: String
input5 = "620080001611562C8802118E34"

input6 :: String
input6 = "C0015000016115A2E0802F182340"

input7 :: String
input7 = "A0016C880162017C3686B18A3D4780"

spec :: Spec Unit
spec =
  describe "Day 16" do
    it "sums version numbers of packet hierarchies" do
      D16.part1 input1 `shouldSucceed` 6
      D16.part1 input2 `shouldSucceed` (1 + 6 + 2)
      D16.part1 input3 `shouldSucceed` (7 + 2 + 4 + 1)
      D16.part1 input4 `shouldSucceed` 16
      D16.part1 input5 `shouldSucceed` 12
      D16.part1 input6 `shouldSucceed` 23
      D16.part1 input7 `shouldSucceed` 31
