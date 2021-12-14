module Advent2021.Spec.Assertions
  ( fromJust
  , shouldParseTo
  , shouldSucceed
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Effect.Exception (Error, error)
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.StringParser (ParseError, printParserError)

fromJust :: forall m a. MonadThrow Error m => Maybe a -> m a
fromJust = maybe (throwError $ error "Maybe evaluated to Nothing") pure

shouldParseTo :: forall a m. Eq a => Show a => MonadThrow Error m => (Either ParseError a) -> a -> m Unit
shouldParseTo = shouldSucceed' printParserError

shouldSucceed :: forall a m. Eq a => Show a => MonadThrow Error m => (Either String a) -> a -> m Unit
shouldSucceed = shouldSucceed' identity

shouldSucceed' :: forall a m e. Eq a => Show a => MonadThrow Error m => (e -> String) -> (Either e a) -> a -> m Unit
shouldSucceed' fmt result expected = case result of
  Right r -> r `shouldEqual` expected
  Left err -> fail $ fmt err
