module Advent2021.Puzzles.D18
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Debug (spy', undefined)
import Advent2021.Parsers (digit, newline, runParser)
import Control.Alternative ((<|>))
import Control.Monad.Except (Except, ExceptT(..), lift, runExcept, runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Either (Either(..))
import Data.List (List(..), Pattern(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Unfoldable (replicate)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepEndBy)

type SnailNumber
  = { left :: SnailElem
    , right :: SnailElem
    }

snailNumberP :: Parser SnailNumber
snailNumberP = do
  _ <- char '['
  left <- snailElemP
  _ <- char ','
  right <- snailElemP
  _ <- char ']'
  pure { left, right }

data SnailElem
  = Pair SnailNumber
  | Regular Int

snailElemP :: Parser SnailElem
snailElemP = pairP <|> regularP
  where
  pairP :: Parser SnailElem
  pairP = do
    _ <- char '['
    left <- snailElemP
    _ <- char ','
    right <- snailElemP
    _ <- char ']'
    pure $ Pair { left, right }

  regularP :: Parser SnailElem
  regularP = Regular <$> digit

inputP :: Parser (List SnailNumber)
inputP = sepEndBy snailNumberP newline <* eof

-- Find the path to exploding pair
-- Construct the expected path prefix
-- Then go until regular number in tail of prefix direction
data Direction
  = ToLeft
  | ToRight

derive instance eqDirection :: Eq Direction

type Path
  = List Direction

explode :: SnailNumber -> Either String (Maybe SnailNumber)
explode snailNumber = case (flip mapExplosion snailNumber) <$> findExplodingPair snailNumber of
  Just (Pair s') -> pure $ Just s'
  Just _ -> Left "Impossible: snail number reduced beyond pair"
  Nothing -> pure Nothing
  where
  doExplosion :: Maybe SnailElem
  doExplosion = do
    result <- findExplodingPair snailNumber
    pure $ mapExplosion result snailNumber

  findExplodingPair :: SnailNumber -> Maybe { explodingPairPath :: Path, explodingPair :: SnailElem }
  findExplodingPair s = findExplodingPairR 1 Nil $ Pair s
    where
    findExplodingPairR depth path pair@(Pair { left, right }) =
      if depth > 4 then
        Just { explodingPairPath: List.reverse path, explodingPair: pair }
      else
        findExplodingPairR (depth + 1) (ToLeft : path) left
          <|> findExplodingPairR (depth + 1) (ToRight : path) right

    findExplodingPairR _ _ (Regular _) = Nothing

  mapExplosion :: { explodingPairPath :: Path, explodingPair :: SnailElem } -> SnailNumber -> SnailElem
  mapExplosion { explodingPairPath, explodingPair: Pair { left: Regular explodingLeft, right: Regular explodingRight } } s = mapExplosionR Nil $ Pair s
    where
    -- Immediate left = find the deepest right turn, go left instead, then keep going right until a regular number
    -- There is no immediate left if and only if the path is all lefts
    pathToImmediateLeft :: Maybe Path
    pathToImmediateLeft =
      spy' "path to immediate left"
        $ do
            i <- List.elemLastIndex ToRight explodingPairPath
            l' <- List.updateAt i ToLeft explodingPairPath
            let
              turnsPrefix = List.take (i + 1) l'
            pure $ turnsPrefix <> replicate 4 ToRight

    pathToImmediateRight :: Maybe Path
    pathToImmediateRight =
      spy' "path to immediate right"
        $ do
            i <- List.elemLastIndex ToLeft explodingPairPath
            l' <- List.updateAt i ToRight explodingPairPath
            let
              turnsPrefix = List.take (i + 1) l'
            pure $ turnsPrefix <> replicate 4 ToLeft

    mapExplosionR :: Path -> SnailElem -> SnailElem
    mapExplosionR path (Pair { left, right }) =
      if path == explodingPairPath then
        Regular 0
      else
        Pair
          { left: mapExplosionR (List.snoc path ToLeft) left
          , right: mapExplosionR (List.snoc path ToRight) right
          }

    mapExplosionR path (Regular r) =
      fromMaybe (Regular r)
        $ checkPathPrefix pathToImmediateLeft explodingLeft
        <|> checkPathPrefix pathToImmediateRight explodingRight
      where
      checkPathPrefix :: Maybe Path -> Int -> Maybe SnailElem
      checkPathPrefix maybePath toAdd = do
        p <- maybePath
        _ <- List.stripPrefix (Pattern path) p
        pure $ Regular $ r + toAdd

  -- SHOULD BE IMPOSSIBLE
  mapExplosion _ s = undefined

addSnail :: SnailNumber -> SnailNumber -> SnailNumber
addSnail a b = { left: Pair a, right: Pair b }
  where
  reduce = undefined

  explode :: SnailNumber -> { result :: SnailNumber, exploded :: Boolean }
  explode = undefined

  split = undefined

examplesInput :: String
examplesInput =
  """[[[[[9,8],1],2],3],4]
[7,[6,[5,[4,[3,2]]]]]
[[6,[5,[4,[3,2]]]],1]
[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]
[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
"""

part1 :: String -> Either String Int
part1 input = do
  numbers <- spy' "numbers" <$> runParser inputP input
  examples <- spy' "examples" <$> runParser inputP examplesInput
  let
    exploded = spy' "exploded" $ explode <$> examples
  pure 0

part2 :: String -> Either String Int
part2 input = pure 0
