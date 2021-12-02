module Advent2021.Puzzles.D2Spec (spec) where

import Prelude

import Advent2021.Puzzles.D2 as D2
import Data.Either (Either(..))
import Data.String (joinWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.StringParser (printParserError)

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
    it "predicts submarine position" do
      case D2.part1 input of
        Right r -> r `shouldEqual` { depth: 10, horizontal: 15 }
        Left err -> fail $ printParserError err
