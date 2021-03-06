-- | The collection of types used in Sudoku.
module Sudoku.Types where

import Prelude
import Data.Array (fold, mapWithIndex)
import Data.Maybe (Maybe(..))

-- | A type representing each cell on a Sudoku board. The first `Int` in the
-- | `Cell` constructor means an index of a cell. The type parameter `a` means
-- | content of a cell, which is usually `Int` for deterministic boards or
-- | `Maybe Int` for nondeterministic ones.
data Cell a = Cell Int a

derive instance eqCellInt :: Eq (Cell Int)
derive instance functorCell :: Functor Cell

instance showCellMaybeInt :: Show (Cell (Maybe Int)) where
  show (Cell _ (Just x)) = show x
  show (Cell _ Nothing) = "x"

instance showCellInt :: Show (Cell Int) where
  show (Cell _ x) = show x

-- | A type representing a row.
newtype Row = Row Int

derive instance rowEq :: Eq Row

-- | A type representing a column.
newtype Col = Col Int

derive instance colEq :: Eq Col

-- | A type representing a board. It has a type parameter `a` meaning the
-- | content of its cells.
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

-- | A type representing a section. A section in Sudoku is a 3x3 area, 9 of
-- | which exist on a board, from `Section 0 0` to `Section 2 2`.
data Section = Section Int Int

-- | A type alias for `Int` representing difficulty of a game. Specifically, it
-- | means the number of holes to fill in.
type Difficulty = Int

-- | A type representing a Sudoku game. It contains its difficulty, question
-- | board and answer board. A game can usually be generated by `generateGame`.
data Game = Game Difficulty (Board (Maybe Int)) (Board Int)

instance showGame :: Show Game where
  show (Game dify ques answ) =
    "Difficulty: " <> show dify <> "\n\n" <>
    "Board:\n" <> show ques <> "\n" <>
    "Answer:\n" <> show answ
