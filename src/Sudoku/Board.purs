module Sudoku.Board where

import Prelude
import Data.Array (snoc, filter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

newtype Cell a = Cell { row :: Int, col :: Int, val :: a }

cell :: forall a. Int -> Int -> a -> Cell a
cell row col val = Cell { row, col, val }

row :: forall a. Cell a -> Int
row (Cell x) = x.row

col :: forall a. Cell a -> Int
col (Cell x) = x.col

derive instance genericCell :: Generic (Cell a) _

instance showCell :: Show a => Show (Cell a) where show = genericShow
instance eqCell :: Eq a => Eq (Cell a) where eq = genericEq

newtype Board a = Board (Array (Cell a))

derive instance genericBoard :: Generic (Board a) _

instance showBoard :: Show a => Show (Board a) where show = genericShow

instance eqBoard :: Eq a => Eq (Board a) where eq = genericEq

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
