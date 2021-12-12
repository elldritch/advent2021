module Test.Main
  ( main
  ) where

import Prelude
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
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
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
