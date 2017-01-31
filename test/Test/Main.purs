module Test.Main where

import Prelude

import Test.Unit.Main (runTest)

import Test.SudokuSpec as Sudoku
import Test.Sudoku.UtilSpec as Util

main = runTest do
  Sudoku.spec
  Util.spec
