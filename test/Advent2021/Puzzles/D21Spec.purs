module Advent2021.Puzzles.D21Spec
  ( spec
  ) where

import Prelude
import Advent2021.Puzzles.D21 as D21
import Advent2021.Spec.Assertions (fromJust, shouldSucceed)
import Data.BigInt as BigInt
import Test.Spec (Spec, describe, it, pending')

input :: String
input =
  """Player 1 starting position: 4
Player 2 starting position: 8
"""

spec :: Spec Unit
spec =
  describe "Day 21" do
    it "plays Dirac Dice with deterministic dice" do
      D21.part1 input `shouldSucceed` 739785
    pending' "plays Dirac Dice with quantum dice" do
      expected <- fromJust $ BigInt.fromString "444356092776315"
      D21.part2 input `shouldSucceed` expected
