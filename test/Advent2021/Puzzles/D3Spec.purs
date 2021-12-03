module Advent2021.Puzzles.D3Spec (spec) where

import Prelude
import Advent2021.Puzzles.D3 as D3
import Advent2021.Spec.Parsers (shouldParseTo)
import Data.String (joinWith)
import Test.Spec (Spec, describe, it)

input :: String
input =
  joinWith "\n"
    [ "00100"
    , "11110"
    , "10110"
    , "10111"
    , "10101"
    , "01111"
    , "00111"
    , "11100"
    , "10000"
    , "11001"
    , "00010"
    , "01010"
    , ""
    ]

spec :: Spec Unit
spec =
  describe "Day 3" do
    it "computes power consumption" do
      D3.part1 input `shouldParseTo` 198
    it "verifies life support rating" do
      D3.part2 input `shouldParseTo` 230
