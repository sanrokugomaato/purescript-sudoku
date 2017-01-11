module Sudoku.Board where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (filter, foldMap, foldRecM, length, range, snoc, updateAt, (!!), (\\))
import Data.Array.Partial (head)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Partial.Unsafe (unsafePartial)

data Cell = Cell Int (Maybe Int)

row :: Cell -> Int
row (Cell i _) = i `div` 9

col :: Cell -> Int
col (Cell i _) = i `mod` 9

value :: Cell -> Maybe Int
value (Cell _ x) = x

instance showCell :: Show Cell where
  show (Cell _ (Just x)) = show x
  show (Cell _ Nothing) = "x"

newtype Board = Board (Array Cell)

instance showBoard :: Show Board where
  show board = foldMap (flip rowAt board >>> foldMap show >>> (<>) "\n") (range 0 8)

emptyBoard :: Board
emptyBoard = Board $ map (\i -> Cell i Nothing) (range 0 80)

rowAt :: Int -> Board -> Array Cell
rowAt i (Board arr) = filter (row >>> eq i) arr

colAt :: Int -> Board -> Array Cell
colAt i (Board arr) = filter (col >>> eq i) arr

sectionAt :: Int -> Int -> Board -> Array Cell
sectionAt secRow secCol (Board arr) = filter f arr
  where
    f cell =
      let r = row cell
          c = col cell
      in
       r >= secRow * 3 && r < (secRow + 1) * 3 &&
       c >= secCol * 3 && c < (secCol + 1) * 3

valueAt :: Int -> Int -> Board -> Maybe Int
valueAt row col (Board arr) = arr !! (row * 9 + col) >>= value

choose :: Array Int -> Eff (random :: RANDOM) Int
choose [x] = pure x
choose xs = do
  idx <- randomInt 0 (length xs - 1)
  pure $ unsafePartial fromJust $ xs !! idx

availableValues :: Int -> Int -> Board -> Array Int
availableValues row col board = range 1 9 \\ already
  where
    already = map (unsafePartial fromJust <<< value)
            $ filter (isJust <<< value)
            $ rowAt row board <> colAt col board <>
              sectionAt (row `div` 3) (col `div` 3) board

replaceValue :: Board -> Int -> Int -> Int -> Board
replaceValue (Board arr) row col val =
  Board $ unsafePartial fromJust $ updateAt idx (Cell idx $ Just val) arr
  where idx = row * 9 + col

insertRandomCell :: Board -> Eff (random :: RANDOM) Board
insertRandomCell board = do
  row <- randomInt 0 8
  col <- randomInt 0 8
  case valueAt row col board of
    Just _ -> insertRandomCell board -- try again
    Nothing -> do
      val <- choose $ availableValues row col board
      pure $ replaceValue board row col val

defaultBoard :: Eff (random :: RANDOM) Board
defaultBoard = foldRecM (\board _ -> insertRandomCell board)
                        emptyBoard
                        (range 1 17)
