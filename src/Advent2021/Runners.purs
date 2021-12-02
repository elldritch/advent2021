module Advent2021.Runners
  ( run
  , runInts
  , runLines
  ) where

import Prelude
import Data.Array.NonEmpty (fromArray, NonEmptyArray)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)

runInts :: forall a. Show a => FilePath -> (NonEmptyArray Int -> a) -> Effect Unit
runInts = withLines asInts
  where
  asInt :: String -> Effect Int
  asInt s = case fromString s of
    Just i -> pure i
    Nothing -> throw $ "Invalid input: expected integer, got " <> show s

  asInts :: NonEmptyArray String -> Effect (NonEmptyArray Int)
  asInts = traverse asInt

runLines :: forall a. Show a => FilePath -> (NonEmptyArray String -> a) -> Effect Unit
runLines = withLines pure

run :: forall a. Show a => FilePath -> (String -> a) -> Effect Unit
run = withInputFile pure

withLines :: forall a b. Show b => (NonEmptyArray String -> Effect a) -> FilePath -> (a -> b) -> Effect Unit
withLines prepare = withInputFile asLines
  where
  asLines contents = case fromArray $ split (Pattern "\n") $ trim contents of
    Just nonEmptyArray -> prepare nonEmptyArray
    Nothing -> throw "Invalid input: input file was empty"

withInputFile :: forall a b. Show b => (String -> Effect a) -> FilePath -> (a -> b) -> Effect Unit
withInputFile prepare inputFilePath solver = do
  contents <- readTextFile UTF8 inputFilePath
  input <- prepare contents
  log $ show $ solver input
