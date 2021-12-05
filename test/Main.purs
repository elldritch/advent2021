module Test.Main
  ( main
  ) where

import Prelude
import Advent2021.Puzzles.D1Spec as D1Spec
import Advent2021.Puzzles.D2Spec as D2Spec
import Advent2021.Puzzles.D3Spec as D3Spec
import Advent2021.Puzzles.D4Spec as D4Spec
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
