module Sudoku.Board where

import Prelude
import Data.Array (filter, foldMap, range, snoc)

newtype Cell a = Cell { row :: Int, col :: Int, val :: a }

cell :: forall a. Int -> Int -> a -> Cell a
cell row col val = Cell { row, col, val }

row :: forall a. Cell a -> Int
row (Cell x) = x.row

col :: forall a. Cell a -> Int
col (Cell x) = x.col

instance showCell :: (Show a) => Show (Cell a) where
  show (Cell cell) = show cell.val

newtype Board a = Board (Array (Cell a))

instance showBoard :: (Show a) => Show (Board a) where
  show board = foldMap (nthRow board >>> foldMap show >>> (<>) "\n") (range 0 8)

createBoard :: forall a. (Board a -> a) -> Board a
createBoard f = go f 0 0 []
  where
    go f row 9 arr = go f (row + 1) 0 arr
    go _ 9 _ arr = Board arr
    go f row col arr = go f row (col + 1)
                       (snoc arr (cell row col (f (Board arr))))

nthRow :: forall a. Board a -> Int -> Array (Cell a)
nthRow (Board arr) i = filter (row >>> eq i) arr

nthCol :: forall a. Board a -> Int -> Array (Cell a)
nthCol (Board arr) i = filter (col >>> eq i) arr
