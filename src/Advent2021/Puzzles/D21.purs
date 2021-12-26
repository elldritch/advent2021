module Advent2021.Puzzles.D21
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Helpers (uniqueCounts)
import Advent2021.Parsers (integer, newline, runParser)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState, evalState, get, put)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either)
import Data.Foldable (sum)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof, string)
import Text.Parsing.StringParser.Combinators (optional)

type StartingPositions
  = { p1 :: Int, p2 :: Int }

inputP :: Parser StartingPositions
inputP = do
  _ <- string "Player 1 starting position: "
  p1 <- integer
  newline
  _ <- string "Player 2 starting position: "
  p2 <- integer
  optional newline
  eof
  pure { p1, p2 }

data Player
  = P1
  | P2

derive instance eqPlayer :: Eq Player

derive instance ordPlayer :: Ord Player

instance showPlayer :: Show Player where
  show P1 = "P1"
  show P2 = "P2"

type PlayerState
  = { position :: Int
    , score :: Int
    }

type Game
  = { player1 :: PlayerState
    , player2 :: PlayerState
    , toPlay :: Player
    }

initialGameState :: StartingPositions -> Game
initialGameState { p1, p2 } =
  { toPlay: P1
  , player1: { position: p1, score: 0 }
  , player2: { position: p2, score: 0 }
  }

deterministicDice :: forall m. MonadState { nextRoll :: Int } m => m Int
deterministicDice = do
  { nextRoll } <- get
  put $ { nextRoll: if nextRoll == 100 then 1 else nextRoll + 1 }
  pure nextRoll

turn :: forall m. Monad m => Game -> m Int -> m Game
turn game@{ toPlay } dice = do
  let
    { selectPlayer, updatePlayer, toPlay' } = case toPlay of
      P1 ->
        { selectPlayer: _.player1
        , updatePlayer: game { player1 = _ }
        , toPlay': P2
        }
      P2 ->
        { selectPlayer: _.player2
        , updatePlayer: game { player2 = _ }
        , toPlay': P1
        }

    { position, score } = selectPlayer game
  position' <- playerTurn position
  pure $ (updatePlayer { position: position', score: score + position' }) { toPlay = toPlay' }
  where
  playerTurn :: Int -> m Int
  playerTurn position = do
    roll1 <- dice
    roll2 <- dice
    roll3 <- dice
    pure $ ((position + roll1 + roll2 + roll3 - 1) `mod` 10) + 1

type GameStep
  = { step :: Game, turns :: Int }

play :: forall m. MonadRec m => m Int -> Game -> m GameStep
play dice initial = tailRecM resultR { step: initial, turns: 0 }
  where
  resultR :: GameStep -> m (Step GameStep GameStep)
  resultR { step: step@{ player1: { score: s1 }, player2: { score: s2 } }, turns } = do
    if s1 >= 1000 || s2 >= 1000 then
      pure $ Done { step, turns: turns }
    else do
      step' <- turn step dice
      pure $ Loop { step: step', turns: turns + 1 }

part1 :: String -> Either String Int
part1 input = do
  start <- runParser inputP input
  let
    { step: { player1: { score: s1 }, player2: { score: s2 }, toPlay }, turns } =
      evalState
        (play deterministicDice $ initialGameState start)
        { nextRoll: 1 }

    diceRolls = turns * 3

    losingScore = case toPlay of
      P1 -> s1
      P2 -> s2
  pure $ losingScore * diceRolls

{-

Part 2 is a totally different algorithm. Paths don't matter, but the count of
paths does.

Keep track of score tuple {p1, p2}. Start at puzzle input positions. How many
ways can one get to {1, 0}? What about {2, 0}? Add up from previous positions.

What about going from {1, 0} to {3, 0}? (Can I just sum path counts here? Or do
I need to account for current player positions, maybe as extra tuple elements?)

(With extra tuple elements, I still only need to track (21, 21, 10, 10) = 44.1k
elements.)

Get sums up until {21, _} or {_, 21}, which is the end of the game.

(How do I do iteration order? I could try to go recursively and pull from the
top, but since that requires multiple pulls (for every {21, _}), it might be
easier to build bottoms-up. Use recursion and enqueue the next quantum rolls.)

(Iterate upwards by points! Use the Z^2 iteration order to make sure none are
missed. This ensures a fixed number of iterations.)

-}
type QuantumGameCount
  = BigInt

type Memo
  = Map Game QuantumGameCount

part2 :: String -> Either String BigInt
part2 input = do
  start <- runParser inputP input
  let
    memo = computeScores start

    p1Wins = countWinningGames P1 memo

    p2Wins = countWinningGames P2 memo
  pure $ max p1Wins p2Wins
  where
  countWinningGames :: Player -> Memo -> BigInt
  countWinningGames player memo =
    sum
      $ Map.values
      $ Map.filterKeys
          ( \{ player1: { score: s1 }, player2: { score: s2 } } -> case player of
              P1 -> s1 == 21
              P2 -> s2 == 21
          )
          memo

  computeScores :: StartingPositions -> Memo
  computeScores start =
    computeScoresR
      (Seq.singleton $ Tuple (initialGameState start) (BigInt.fromInt 1))
      $ Map.empty

  computeScoresR :: Seq (Tuple Game QuantumGameCount) -> Memo -> Memo
  computeScoresR queue memo = case Seq.uncons queue of
    Just (Tuple (Tuple game count) rest) ->
      if isCompleted game then
        computeScoresR rest withGame
      else
        computeScoresR (rest `Seq.append` Map.toUnfoldable reachable) withGame
      where
      withGame = Map.insertWith (+) game count memo

      reachable = (_ * count) <$> nextPositions game
    Nothing -> memo

  nextPositions :: Game -> Map Game QuantumGameCount
  nextPositions step = uniqueCounts $ turn step $ List.range 1 3

  isCompleted :: Game -> Boolean
  isCompleted { player1: { score: s1 }, player2: { score: s2 } } = s1 >= 21 || s2 >= 21
