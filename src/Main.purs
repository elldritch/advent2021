module Main
  ( main
  ) where

import Prelude
import Advent2021.D1 (part1)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Array.NonEmpty (fromArray)
import Data.Array.Partial (init)
import Data.Int (fromString)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), split)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  contents <- readTextFile UTF8 "./inputs/1/1"
  let
    depths =
      unsafePartial
        $ fromJust
        $ fromArray
        $ map (fromJust <<< fromString)
        $ init
        $ split (Pattern "\n") contents
  log $ show $ part1 depths
