module Test.Main
  ( main
  ) where

import Prelude
import Advent2021.Puzzles.D1 as D1
import Data.Array.NonEmpty (cons')
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Day 1" do
          it "checks depth increases" do
            let
              input =
                199
                  `cons'`
                    [ 200
                    , 208
                    , 210
                    , 200
                    , 207
                    , 240
                    , 269
                    , 260
                    , 263
                    ]

              expected = 7

              actual = D1.part1 input
            actual `shouldEqual` expected
