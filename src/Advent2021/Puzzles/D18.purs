module Advent2021.Puzzles.D18
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Debug (spy', undefined)
import Advent2021.Parsers (digit, integer, newline, runParser)
import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), Pattern(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Unfoldable (replicate)
import Effect.Exception.Unsafe (unsafeThrow)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepEndBy)

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

snailNumberP :: Parser SnailfishNumber
snailNumberP = SnailfishNumber <$> pairP snailfishElementP

data SnailfishElement
  = Pair (Pair SnailfishElement)
  | Regular Int

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

inputP :: Parser (List SnailfishNumber)
inputP = sepEndBy snailNumberP newline <* eof

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

explode :: SnailfishNumber -> Maybe SnailfishNumber
explode snailfishNumber = do
  explodingPair <- findExplodingPair snailfishNumber
  pure $ mapExplosion explodingPair snailfishNumber
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
    SnailfishNumber
  mapExplosion { explodingPairPath, explodingPair: { left: explodingLeft, right: explodingRight } } (SnailfishNumber s) = case mapExplosionR Nil $ Pair s of
    Pair p -> SnailfishNumber p
    Regular _ -> unsafeThrow "Impossible: root of exploded expression was not a pair"
    where
    sibling :: Direction -> Maybe Path
    sibling direction = do
      i <- List.elemLastIndex (opposite direction) explodingPairPath
      l' <- List.updateAt i direction explodingPairPath
      let
        turnsPrefix = List.take (i + 1) l'
      pure $ turnsPrefix <> replicate 4 (opposite direction)

    -- Immediate left = find the deepest right turn, go left instead, then keep going right until a regular number
    -- There is no immediate left if and only if the path is all lefts
    pathToImmediateLeft :: Maybe Path
    pathToImmediateLeft = spy' "path to immediate left" $ sibling ToLeft

    pathToImmediateRight :: Maybe Path
    pathToImmediateRight = spy' "path to immediate right" $ sibling ToRight

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

split :: SnailfishNumber -> Maybe SnailfishNumber
split (SnailfishNumber s) =
  let
    { splitted, element } = splitR false $ Pair s
  in
    if splitted then case element of
      Pair s' -> Just $ SnailfishNumber s'
      _ -> unsafeThrow "Impossible: root of split expression was not a pair"
    else
      Nothing
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

reduce :: SnailfishNumber -> SnailfishNumber
reduce = undefined

addSnail :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
addSnail (SnailfishNumber a) (SnailfishNumber b) = SnailfishNumber { left: Pair a, right: Pair b }
  where
  reduce = undefined

  explode :: SnailfishNumber -> { result :: SnailfishNumber, exploded :: Boolean }
  explode = undefined

  split = undefined

explodeExamplesInput :: String
explodeExamplesInput =
  """[[[[[9,8],1],2],3],4]
[7,[6,[5,[4,[3,2]]]]]
[[6,[5,[4,[3,2]]]],1]
[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]
[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
[[[[0,7],4],[[7,8],[6,0]]],[8,1]]
"""

splitExamplesInput :: String
splitExamplesInput =
  """[[[[0,7],4],[15,[0,13]]],[1,1]]
[[[[0,7],4],[[7,8],[0,13]]],[1,1]]
[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
"""

part1 :: String -> Either String Int
part1 input = do
  numbers <- spy' "numbers" <$> runParser inputP input
  explodeExamples <- spy' "explodeExamples" <$> runParser inputP explodeExamplesInput
  let
    exploded = spy' "exploded" $ explode <$> explodeExamples
  splitExamples <- spy' "splitExamples" <$> runParser inputP splitExamplesInput
  let
    splitted = spy' "splitted" $ split <$> splitExamples
  pure 0

part2 :: String -> Either String Int
part2 input = pure 0
