module Sudoku.Types where

import Prelude
import Data.Array (fold, mapWithIndex)
import Data.Maybe (Maybe(..))

data Cell a = Cell Int a

derive instance eqCellInt :: Eq (Cell Int)
derive instance functorCell :: Functor Cell

instance showCellMaybeInt :: Show (Cell (Maybe Int)) where
  show (Cell _ (Just x)) = show x
  show (Cell _ Nothing) = "x"

instance showCellInt :: Show (Cell Int) where
  show (Cell _ x) = show x

newtype Row = Row Int

derive instance rowEq :: Eq Row

newtype Col = Col Int

derive instance colEq :: Eq Col

newtype Board a = Board (Array (Cell a))

derive instance eqBoardInt :: Eq (Board Int)
derive instance functorBoard :: Functor Board

instance showBoard :: (Show (Cell a)) => Show (Board a) where
  show (Board arr) = fold $ mapWithIndex showAndBreak arr
    where
      showAndBreak :: Int -> Cell a -> String
      showAndBreak idx cell
        | idx `mod` 9 == 8 = show cell <> "\n"
        | otherwise = show cell

data Section = Section Int Int -- Section 0 0 ~ Section 2 2

type Difficulty = Int

data Game = Game Difficulty (Board (Maybe Int)) (Board Int)

instance showGame :: Show Game where
  show (Game dify ques answ) =
    "Difficulty: " <> show dify <> "\n\n" <>
    "Board:\n" <> show ques <> "\n" <>
    "Answer:\n" <> show answ
