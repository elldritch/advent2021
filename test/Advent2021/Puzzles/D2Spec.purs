module Advent2021.Puzzles.D2Spec (spec) where

import Prelude
import Advent2021.Puzzles.D2 as D2
import Advent2021.Spec.Assertions (shouldParseTo)
import Data.String (joinWith)
import Test.Spec (Spec, describe, it)

input :: String
input =
  joinWith "\n"
    [ "forward 5"
    , "down 5"
    , "forward 8"
    , "up 3"
    , "down 8"
    , "forward 2"
    , ""
    ]

spec :: Spec Unit
spec =
  describe "Day 2" do
    it "moves the submarine" do
      D2.part1 input `shouldParseTo` 150
    it "aims the submarine" do
      D2.part2 input `shouldParseTo` 900
