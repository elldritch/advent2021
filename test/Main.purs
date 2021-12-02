module Test.Main
  ( main
  ) where

import Prelude
import Advent2021.Puzzles.D1Spec as D1Spec
import Advent2021.Puzzles.D2Spec as D2Spec
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
