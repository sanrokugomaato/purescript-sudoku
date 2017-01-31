module Test.Main where

import Prelude

import Test.Unit.Main (runTest)

import Test.Sudoku.UtilSpec as Util

main = runTest do
  Util.spec
