module Test.Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Advent2021.Puzzles.D1Spec as D1Spec

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        D1Spec.spec
