module Test.Main
  ( main
  ) where

import Prelude
import Advent2021.BitsSpec as BitsSpec
import Advent2021.Puzzles.D1Spec as D1Spec
import Advent2021.Puzzles.D2Spec as D2Spec
import Advent2021.Puzzles.D3Spec as D3Spec
import Advent2021.Puzzles.D4Spec as D4Spec
import Advent2021.Puzzles.D5Spec as D5Spec
import Advent2021.Puzzles.D6Spec as D6Spec
import Advent2021.Puzzles.D7Spec as D7Spec
import Advent2021.Puzzles.D8Spec as D8Spec
import Advent2021.Puzzles.D9Spec as D9Spec
import Advent2021.Puzzles.D10Spec as D10Spec
import Advent2021.Puzzles.D11Spec as D11Spec
import Advent2021.Puzzles.D12Spec as D12Spec
import Advent2021.Puzzles.D13Spec as D13Spec
import Advent2021.Puzzles.D14Spec as D14Spec
import Advent2021.Puzzles.D15Spec as D15Spec
import Advent2021.Puzzles.D16Spec as D16Spec
import Advent2021.Puzzles.D17Spec as D17Spec
import Advent2021.Puzzles.D18Spec as D18Spec
import Advent2021.Puzzles.D19Spec as D19Spec
import Advent2021.Puzzles.D20Spec as D20Spec
import Advent2021.Puzzles.D21Spec as D21Spec
import Advent2021.Puzzles.D22Spec as D22Spec
import Advent2021.Puzzles.D23Spec as D23Spec
import Advent2021.Puzzles.D24Spec as D24Spec
import Advent2021.Puzzles.D25Spec as D25Spec
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Utilities" do
          BitsSpec.spec
        describe "Puzzles" do
          D1Spec.spec
          D2Spec.spec
          D3Spec.spec
          D4Spec.spec
          D5Spec.spec
          D6Spec.spec
          D7Spec.spec
          D8Spec.spec
          D9Spec.spec
          D10Spec.spec
          D11Spec.spec
          D12Spec.spec
          D13Spec.spec
          D14Spec.spec
          D15Spec.spec
          D16Spec.spec
          D17Spec.spec
          D18Spec.spec
          D19Spec.spec
          D20Spec.spec
          D21Spec.spec
          D22Spec.spec
          D23Spec.spec
          D24Spec.spec
          D25Spec.spec
