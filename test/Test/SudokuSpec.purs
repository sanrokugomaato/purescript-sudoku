module Test.SudokuSpec where

import Prelude
import Sudoku
import Sudoku.Types
import Control.Monad.Eff.Class (liftEff)
import Data.Array (length, (..), sort)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, equal)

checkComplete board = do
  traverse_ (fullCheck <<< flip rowCells board <<< Row) (0..8)
  traverse_ (fullCheck <<< flip colCells board <<< Col) (0..8)
  traverse_ (fullCheck <<< flip sectionCells board) do
    x <- 0..2
    y <- 0..2
    pure $ Section x y
  where
  fullCheck cells = do
    (1..9) `equal` sort (map valueOf cells)

spec = suite "Test.SudokuSpec" do
  suite "emptyBoard" do
    test "length" do
      let xs = case emptyBoard of Board xs -> xs
      length xs `equal` 81

    test "emptiness" do
      let xs = case emptyBoard of Board xs -> xs
      traverse_ (equal Nothing <<< valueOf) xs

  suite "fullBoard" do
    test "length" do
      Board xs <- liftEff fullBoard
      length xs `equal` 81

    test "completeness" do
      board <- liftEff fullBoard
      checkComplete board

  suite "generateGame" do
    let minDify = 40

    test "difficulty" do
      Game dify _ _ <- liftEff $ generateGame minDify
      assert "difficulty" $ dify >= minDify

    test "answer completeness" do
      Game _ _ answer <- liftEff $ generateGame minDify
      checkComplete answer

    test "solvability" do
      Game _ question answer <- liftEff $ generateGame minDify
      let solutions = solve question
      assert "unique" $ length solutions == 1
      assert "right" $ solutions == [answer]
