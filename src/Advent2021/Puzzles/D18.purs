module Advent2021.Puzzles.D18
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Helpers (fixM)
import Advent2021.Parsers (integer, newline, runParser)
import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.List (List(..), Pattern(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Unfoldable (replicate)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepEndBy1)

type Pair a
  = { left :: a, right :: a }

pairP :: forall a. Parser a -> Parser (Pair a)
pairP p = do
  _ <- char '['
  left <- p
  _ <- char ','
  right <- p
  _ <- char ']'
  pure $ { left, right }

newtype SnailfishNumber
  = SnailfishNumber (Pair SnailfishElement)

derive instance eqSnailfishNumber :: Eq SnailfishNumber

snailNumberP :: Parser SnailfishNumber
snailNumberP = SnailfishNumber <$> pairP snailfishElementP

data SnailfishElement
  = Pair (Pair SnailfishElement)
  | Regular Int

derive instance eqSnailfishElement :: Eq SnailfishElement

snailfishElementP :: Parser SnailfishElement
snailfishElementP = pairP' <|> regularP
  where
  pairP' :: Parser SnailfishElement
  pairP' = do
    _ <- char '['
    left <- snailfishElementP
    _ <- char ','
    right <- snailfishElementP
    _ <- char ']'
    pure $ Pair { left, right }

  regularP :: Parser SnailfishElement
  regularP = Regular <$> integer

inputP :: Parser (NonEmptyList SnailfishNumber)
inputP = sepEndBy1 snailNumberP newline <* eof

-- Find the path to exploding pair
-- Construct the expected path prefix
-- Then go until regular number in tail of prefix direction
data Direction
  = ToLeft
  | ToRight

opposite :: Direction -> Direction
opposite ToLeft = ToRight

opposite ToRight = ToLeft

derive instance eqDirection :: Eq Direction

type Path
  = List Direction

explode :: SnailfishNumber -> Either String (Maybe SnailfishNumber)
explode snailfishNumber = case findExplodingPair snailfishNumber of
  Just explodingPair -> Just <$> mapExplosion explodingPair snailfishNumber
  Nothing -> pure Nothing
  where
  findExplodingPair :: SnailfishNumber -> Maybe { explodingPairPath :: Path, explodingPair :: Pair Int }
  findExplodingPair (SnailfishNumber s) = findExplodingPairR 1 Nil $ Pair s
    where
    findExplodingPairR :: Int -> Path -> SnailfishElement -> Maybe { explodingPairPath :: Path, explodingPair :: Pair Int }
    findExplodingPairR depth path (Pair { left: Regular left, right: Regular right }) =
      if depth > 4 then
        Just { explodingPairPath: List.reverse path, explodingPair: { left, right } }
      else
        Nothing

    findExplodingPairR depth path (Pair { left, right }) =
      findExplodingPairR (depth + 1) (ToLeft : path) left
        <|> findExplodingPairR (depth + 1) (ToRight : path) right

    findExplodingPairR _ _ (Regular _) = Nothing

  mapExplosion ::
    { explodingPairPath :: Path, explodingPair :: Pair Int } ->
    SnailfishNumber ->
    Either String SnailfishNumber
  mapExplosion { explodingPairPath, explodingPair: { left: explodingLeft, right: explodingRight } } (SnailfishNumber s) = case mapExplosionR Nil $ Pair s of
    Pair p -> pure $ SnailfishNumber p
    Regular _ -> Left "Impossible: root of exploded expression was not a pair"
    where
    -- Immediate left = find the deepest right turn, go left instead, then keep
    -- going right until a regular number. There is no immediate left if and
    -- only if the path is all lefts.
    --
    -- Vice versa for immediate right.
    sibling :: Direction -> Maybe Path
    sibling direction = do
      i <- List.elemLastIndex (opposite direction) explodingPairPath
      l' <- List.updateAt i direction explodingPairPath
      let
        turnsPrefix = List.take (i + 1) l'
      -- Replicate 4 because pairs are nested 4 deep at most. Alternatively, use
      -- an infinite lazy list here.
      pure $ turnsPrefix <> replicate 4 (opposite direction)

    pathToImmediateLeft :: Maybe Path
    pathToImmediateLeft = sibling ToLeft

    pathToImmediateRight :: Maybe Path
    pathToImmediateRight = sibling ToRight

    mapExplosionR :: Path -> SnailfishElement -> SnailfishElement
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
      checkPathPrefix :: Maybe Path -> Int -> Maybe SnailfishElement
      checkPathPrefix maybePath toAdd = do
        p <- maybePath
        _ <- List.stripPrefix (Pattern path) p
        pure $ Regular $ r + toAdd

split :: SnailfishNumber -> Either String (Maybe SnailfishNumber)
split (SnailfishNumber s) =
  let
    { splitted, element } = splitR false $ Pair s
  in
    if splitted then case element of
      Pair s' -> pure $ Just $ SnailfishNumber s'
      _ -> Left "Impossible: root of split expression was not a pair"
    else
      pure $ Nothing
  where
  splitR :: Boolean -> SnailfishElement -> { splitted :: Boolean, element :: SnailfishElement }
  splitR true x = { splitted: true, element: x }

  splitR false (Pair { left, right }) =
    { splitted: splittedR
    , element: Pair { left: elementL, right: elementR }
    }
    where
    { splitted: splittedL, element: elementL } = splitR false left

    { splitted: splittedR, element: elementR } = splitR splittedL right

  splitR false (Regular r) =
    if r >= 10 then
      { splitted: true
      , element:
          Pair
            { left: Regular $ r / 2
            , right: Regular $ r / 2 + r `mod` 2
            }
      }
    else
      { splitted: false, element: Regular r }

reduce :: SnailfishNumber -> Either String SnailfishNumber
reduce = fixM reduce'
  where
  reduce' s = do
    exploded <- explode s
    splitted <- split s
    pure $ fromMaybe s $ exploded <|> splitted

snailfishAdd :: SnailfishNumber -> SnailfishNumber -> Either String SnailfishNumber
snailfishAdd (SnailfishNumber a) (SnailfishNumber b) =
  reduce
    $ SnailfishNumber
        { left: Pair a
        , right: Pair b
        }

magnitude :: SnailfishNumber -> Int
magnitude (SnailfishNumber s) = magnitudeR $ Pair s
  where
  magnitudeR :: SnailfishElement -> Int
  magnitudeR (Pair { left, right }) = 3 * magnitudeR left + 2 * magnitudeR right

  magnitudeR (Regular r) = r

part1 :: String -> Either String Int
part1 input = do
  numbers <- runParser inputP input
  result <- foldM snailfishAdd (NEList.head numbers) $ NEList.tail numbers
  pure $ magnitude result

part2 :: String -> Either String Int
part2 input = pure 0
