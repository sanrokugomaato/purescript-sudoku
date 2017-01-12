module Sudoku where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (filter, foldMap, foldRecM, length, range, snoc, updateAt, (!!), (\\))
import Data.Array.Partial (head)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

data Cell = Cell Int (Maybe Int)

type Row = Int
type Col = Int

row :: Cell -> Row
row (Cell i _) = i `div` 9

col :: Cell -> Col
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

rowAt :: Row -> Board -> Array Cell
rowAt i (Board arr) = filter (row >>> eq i) arr

colAt :: Col -> Board -> Array Cell
colAt i (Board arr) = filter (col >>> eq i) arr

sectionAt :: Row -> Col -> Board -> Array Cell
sectionAt secRow secCol (Board arr) = filter f arr
  where
    f cell =
      let r = row cell
          c = col cell
      in
       r >= secRow * 3 && r < (secRow + 1) * 3 &&
       c >= secCol * 3 && c < (secCol + 1) * 3

valueAt :: Row -> Col -> Board -> Maybe Int
valueAt row col (Board arr) = arr !! (row * 9 + col) >>= value

choose :: Array Int -> Eff (random :: RANDOM) Int
choose [x] = pure x
choose xs = do
  idx <- randomInt 0 (length xs - 1)
  pure $ unsafePartial fromJust $ xs !! idx

availableValues :: Row -> Col -> Board -> Array Int
availableValues row col board = range 1 9 \\ already
  where
    already = map (unsafePartial fromJust <<< value)
            $ filter (isJust <<< value)
            $ rowAt row board <> colAt col board <>
              sectionAt (row `div` 3) (col `div` 3) board

replaceValue :: Row -> Col -> Maybe Int -> Board -> Board
replaceValue row col val (Board arr) =
  Board $ unsafePartial fromJust $ updateAt idx (Cell idx val) arr
  where idx = row * 9 + col

emptyRandomCell :: Board -> Eff (random :: RANDOM) Board
emptyRandomCell board = do
  row <- randomInt 0 8
  col <- randomInt 0 8
  case valueAt row col board of
    Nothing -> emptyRandomCell board -- try again, not smart but works
    Just _ -> pure $ replaceValue row col Nothing board

fullBoard :: Eff (random :: RANDOM) Board
fullBoard = unsafePartial fromJust <$> go 0 0 [] emptyBoard
  where
    go :: Row -> Col -> Array Int -> Board -> Eff (random :: RANDOM) (Maybe Board)
    go row 9 _ board = go (row + 1) 0 [] board
    go 9 _ _ board = pure $ Just board
    go row col alreadyTried board =
      case availableValues row col board \\ alreadyTried of
        [] -> pure Nothing
        xs -> do
          curVal <- choose xs
          let curBoard = replaceValue row col (Just curVal) board
          nextBoard <- go row (col + 1) [] curBoard
          case nextBoard of
            Nothing -> go row col (snoc alreadyTried curVal) board
            justBoard -> pure justBoard

solve :: Board -> Array Board
solve board = go 0 0 board
  where
    go :: Row -> Col -> Board -> Array Board
    go row 9 board = go (row + 1) 0 board
    go 9 _ board = [board]
    go row col board =
      case valueAt row col board of
        Just _ -> go row (col + 1) board
        Nothing -> flip foldMap (availableValues row col board) \val ->
          go row (col + 1) $ replaceValue row col (Just val) board

type Difficulty = Int

generateGame :: Difficulty -> Eff (random :: RANDOM) (Tuple Difficulty Board)
generateGame minDify = fullBoard >>= go 0
  where
  go :: Difficulty -> Board -> Eff (random :: RANDOM) (Tuple Difficulty Board)
  go dify old = do
    new <- emptyRandomCell old
    case length $ solve new of
      0 -> go dify old -- try empty again
      1 -> go (dify + 1) new -- try more
      _ -> if dify > minDify
           then pure $ Tuple dify old -- return the game
           else generateGame minDify -- try from scratch! :sad:
