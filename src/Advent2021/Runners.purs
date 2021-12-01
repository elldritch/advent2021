module Advent2021.Runners
  ( runInts
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Data.Array.NonEmpty (fromArray, NonEmptyArray)
import Data.Array.Partial (init)
import Data.Int (fromString)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), split)
import Partial.Unsafe (unsafePartial)

runInts :: forall a. Show a => FilePath -> (NonEmptyArray Int -> a) -> Effect Unit
runInts inputFile solver = do
  contents <- readTextFile UTF8 inputFile
  let
    input =
      unsafePartial
        $ fromJust
        $ fromArray
        $ map (fromJust <<< fromString)
        $ init
        $ split (Pattern "\n") contents
  log $ show $ solver input
