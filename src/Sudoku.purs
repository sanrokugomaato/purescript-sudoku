module Sudoku where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (filter, foldMap, length, range, snoc, updateAt, (!!), (\\))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

data Cell = Cell Int (Maybe Int)

newtype Row = Row Int
newtype Col = Col Int
derive instance rowEq :: Eq Row
derive instance colEq :: Eq Col

rowOf :: Cell -> Row
rowOf (Cell i _) = Row $ i `div` 9

colOf :: Cell -> Col
colOf (Cell i _) = Col $ i `mod` 9

valueOf :: Cell -> Maybe Int
valueOf (Cell _ x) = x

instance showCell :: Show Cell where
  show (Cell _ (Just x)) = show x
  show (Cell _ Nothing) = "x"

newtype Board = Board (Array Cell)

instance showBoard :: Show Board where
  show board = foldMap (Row >>> flip rowCells board >>> foldMap show >>> (<>) "\n") (range 0 8)

emptyBoard :: Board
emptyBoard = Board $ map (\i -> Cell i Nothing) (range 0 80)

rowCells :: Row -> Board -> Array Cell
rowCells i (Board arr) = filter (rowOf >>> eq i) arr

colCells :: Col -> Board -> Array Cell
colCells i (Board arr) = filter (colOf >>> eq i) arr

data Section = Section Int Int -- Section 0 0 ~ Section 2 2

sectionOf :: Row -> Col -> Section
sectionOf (Row r) (Col c) = Section (r `div` 3) (c `div` 3)

contains :: Section -> Cell -> Boolean
contains (Section secRow secCol) cell =
  let
    rowIdx = case rowOf cell of Row i -> i
    colIdx = case colOf cell of Col i -> i
  in
   rowIdx >= secRow * 3 && rowIdx < (secRow + 1) * 3 &&
   colIdx >= secCol * 3 && colIdx < (secCol + 1) * 3

sectionCells :: Section -> Board -> Array Cell
sectionCells sec (Board arr) = filter (contains sec) arr

rawIdx :: Row -> Col -> Int
rawIdx (Row r) (Col c) = r * 9 + c

valueAt :: Row -> Col -> Board -> Maybe Int
valueAt row col (Board arr) = arr !! rawIdx row col >>= valueOf

choose :: forall e. Array Int -> Eff (random :: RANDOM | e) Int
choose [x] = pure x
choose xs = do
  idx <- randomInt 0 (length xs - 1)
  pure $ unsafePartial fromJust $ xs !! idx

availableValues :: Row -> Col -> Board -> Array Int
availableValues row col board = range 1 9 \\ already
  where
    already = map (unsafePartial fromJust <<< valueOf)
            $ filter (isJust <<< valueOf)
            $ rowCells row board <> colCells col board <>
              sectionCells (sectionOf row col) board

replaceValue :: Row -> Col -> Maybe Int -> Board -> Board
replaceValue row col val (Board arr) =
  Board $ unsafePartial fromJust $ updateAt idx (Cell idx val) arr
  where idx = rawIdx row col

emptyRandomCell :: forall e. Board -> Eff (random :: RANDOM | e) Board
emptyRandomCell board = do
  row <- Row <$> randomInt 0 8
  col <- Col <$> randomInt 0 8
  case valueAt row col board of
    Nothing -> emptyRandomCell board -- try again, not smart but works
    Just _ -> pure $ replaceValue row col Nothing board

fullBoard :: forall e. Eff (random :: RANDOM | e) Board
fullBoard = unsafePartial fromJust <$> go (Row 0) (Col 0) [] emptyBoard
  where
    go :: Row -> Col -> Array Int -> Board -> Eff (random :: RANDOM | e) (Maybe Board)
    go (Row r) (Col 9) _ board = go (Row $ r + 1) (Col 0) [] board
    go (Row 9) _ _ board = pure $ Just board
    go row col@(Col c) alreadyTried board =
      case availableValues row col board \\ alreadyTried of
        [] -> pure Nothing
        xs -> do
          curVal <- choose xs
          let curBoard = replaceValue row col (Just curVal) board
          nextBoard <- go row (Col $ c + 1) [] curBoard
          case nextBoard of
            Nothing -> go row col (snoc alreadyTried curVal) board
            justBoard -> pure justBoard

solve :: Board -> Array Board
solve = go (Row 0) (Col 0)
  where
    go :: Row -> Col -> Board -> Array Board
    go (Row r) (Col 9) board = go (Row $ r + 1) (Col 0) board
    go (Row 9) _ board = [board]
    go row col@(Col c) board =
      case valueAt row col board of
        Just _ -> go row (Col $ c + 1) board
        Nothing -> flip foldMap (availableValues row col board) \val ->
          go row (Col $ c + 1) $ replaceValue row col (Just val) board

type Difficulty = Int

generateGame :: forall e. Difficulty -> Eff (random :: RANDOM | e) (Tuple Difficulty Board)
generateGame minDify = fullBoard >>= go 0
  where
    go :: Difficulty -> Board -> Eff (random :: RANDOM | e) (Tuple Difficulty Board)
    go dify old = do
      new <- emptyRandomCell old
      case length $ solve new of
        0 -> go dify old -- try empty again
        1 -> go (dify + 1) new -- try more
        _ -> if dify > minDify
            then pure $ Tuple dify old -- return the game
            else generateGame minDify -- try from scratch! :sad:
