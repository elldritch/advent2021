module Advent2021.Spec.Assertions
  ( shouldParseTo
  , shouldSucceed
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.StringParser (ParseError, printParserError)

shouldParseTo :: forall a m. Eq a => Show a => MonadThrow Error m => (Either ParseError a) -> a -> m Unit
shouldParseTo = shouldSucceed' printParserError

shouldSucceed :: forall a m. Eq a => Show a => MonadThrow Error m => (Either String a) -> a -> m Unit
shouldSucceed = shouldSucceed' identity

shouldSucceed' :: forall a m e. Eq a => Show a => MonadThrow Error m => (e -> String) -> (Either e a) -> a -> m Unit
shouldSucceed' fmt result expected = case result of
  Right r -> r `shouldEqual` expected
  Left err -> fail $ fmt err
