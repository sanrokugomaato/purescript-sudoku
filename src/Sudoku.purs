-- | This is the main module of Sudoku. It provides a set of functions to
-- | generate, manipulate or solve a Sudoku game.
module Sudoku where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (filter, foldMap, length, range, snoc, updateAt, (!!), (\\))
import Data.Maybe (Maybe(..), isJust)
import Sudoku.Types (Cell(..), Row(..), Col(..), Board(..), Section(..), Difficulty, Game(..))
import Sudoku.Util (fromJust', choose)

-- | The `emptyBoard` function creates an empty board filled with `Nothing`.
emptyBoard :: Board (Maybe Int)
emptyBoard = Board $ map (\i -> Cell i Nothing) (range 0 80)

-- | The `fullBoard` function creates a random complete answer for a Sudoku
-- | game.
fullBoard :: forall e. Eff (random :: RANDOM | e) (Board Int)
fullBoard = fromJust' <$> go (Row 0) (Col 0) [] emptyBoard
  where
    go :: Row -> Col -> Array Int -> Board (Maybe Int)-> Eff (random :: RANDOM | e) (Maybe (Board Int))
    go (Row r) (Col 9) _ board = go (Row $ r + 1) (Col 0) [] board
    go (Row 9) _ _ board = pure <<< Just $ fromJust' <$> board
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

-- | The `rowOf` function calculates which row a cell belongs to.
rowOf :: forall a. Cell a -> Row
rowOf (Cell i _) = Row $ i `div` 9

-- | The `colOf` function calculates which column a cell belongs to.
colOf :: forall a. Cell a -> Col
colOf (Cell i _) = Col $ i `mod` 9

-- | The `valueOf` function calculates which value a cell has.
valueOf :: forall a. Cell a -> a
valueOf (Cell _ x) = x

-- | The `rowCells` function retrieves an array of cells which belong to a
-- | specfic row on a board.
rowCells :: forall a. Row -> Board a -> Array (Cell a)
rowCells i (Board arr) = filter (rowOf >>> eq i) arr

-- | The `colCells` function retrieves an array of cells which belong to a
-- | specfic column on a board.
colCells :: forall a. Col -> Board a -> Array (Cell a)
colCells i (Board arr) = filter (colOf >>> eq i) arr

-- | The `sectionAt` function calculates which section a row/column pair belongs
-- | to.
sectionAt :: Row -> Col -> Section
sectionAt (Row r) (Col c) = Section (r `div` 3) (c `div` 3)

-- | The `sectionContains` function tells if a section contains a cell.
sectionContains :: forall a. Section -> Cell a -> Boolean
sectionContains (Section secRow secCol) cell =
  let
    rowIdx = case rowOf cell of Row i -> i
    colIdx = case colOf cell of Col i -> i
  in
   rowIdx >= secRow * 3 && rowIdx < (secRow + 1) * 3 &&
   colIdx >= secCol * 3 && colIdx < (secCol + 1) * 3

-- | The `sectionCells` function retrieves an array of cells which belong to a
-- | specific section on a board.
sectionCells :: forall a. Section -> Board a -> Array (Cell a)
sectionCells sec (Board arr) = filter (sectionContains sec) arr

-- | The `idxAt` function converts a row/column pair into a corresponding index
-- | in an internal array of `Board`.
idxAt :: Row -> Col -> Int
idxAt (Row r) (Col c) = r * 9 + c

-- | The `valueAt` function retrieves a value of a cell with a row and column on
-- | a board.
valueAt :: forall a. Row -> Col -> Board a -> a
valueAt row col (Board arr) = valueOf $ fromJust' (arr !! idxAt row col)

-- | The `availableValues` function calculates values available at a cell with
-- | the Sudoku rule.
availableValues :: Row -> Col -> Board (Maybe Int) -> Array Int
availableValues row col board = range 1 9 \\ already
  where
    already = map (fromJust' <<< valueOf)
            $ filter (isJust <<< valueOf)
            $ rowCells row board <> colCells col board <>
              sectionCells (sectionAt row col) board

-- | The `replaceValue` function replaces a value at a cell at a row/column pair
-- | on a board.
replaceValue :: forall a. Row -> Col -> a -> Board a -> Board a
replaceValue row col val (Board arr) =
  Board $ fromJust' $ updateAt idx (Cell idx val) arr
  where idx = idxAt row col

-- | The `generateGame` function generates a random Sudoku game with a minimum
-- | difficulty.
generateGame :: forall e. Difficulty -> Eff (random :: RANDOM | e) Game
generateGame minDify = do
  answer <- fullBoard
  go 0 answer (Just <$> answer)
  where
    emptyRandomCell :: Board (Maybe Int) -> Eff (random :: RANDOM | e) (Board (Maybe Int))
    emptyRandomCell board = do
      row <- Row <$> randomInt 0 8
      col <- Col <$> randomInt 0 8
      case valueAt row col board of
        Nothing -> emptyRandomCell board -- try again, not smart but works
        Just _ -> pure $ replaceValue row col Nothing board

    go :: Difficulty -> Board Int -> Board (Maybe Int) -> Eff (random :: RANDOM | e) Game
    go dify answer old = do
      new <- emptyRandomCell old
      case length $ solve new of
        0 -> go dify answer old -- try empty again
        1 -> go (dify + 1) answer new -- try more
        _ -> if dify > minDify
            then pure $ Game dify old answer -- return the game
            else generateGame minDify -- try from scratch! :sad:

-- | The `solve` function literally solves a Sudoku question board, returns an
-- | array of complete solutions.
solve :: Board (Maybe Int) -> Array (Board Int)
solve = go (Row 0) (Col 0)
  where
    go :: Row -> Col -> Board (Maybe Int) -> Array (Board Int)
    go (Row r) (Col 9) board = go (Row $ r + 1) (Col 0) board
    go (Row 9) _ board = [fromJust' <$> board]
    go row col@(Col c) board =
      case valueAt row col board of
        Just _ -> go row (Col $ c + 1) board
        Nothing -> flip foldMap (availableValues row col board) \val ->
          go row (Col $ c + 1) $ replaceValue row col (Just val) board
