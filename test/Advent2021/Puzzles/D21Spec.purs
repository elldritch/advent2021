module Advent2021.Puzzles.D21Spec
  ( spec
  ) where

import Prelude
import Advent2021.Puzzles.D21 as D21
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input =
  """Player 1 starting position: 4
Player 2 starting position: 8
"""

spec :: Spec Unit
spec =
  describe "Day 21" do
    it "plays Dirac Dice" do
      D21.part1 input `shouldSucceed` 739785
