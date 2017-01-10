module Sudoku.BoardSpec where

import Prelude

import Test.Spec
import Test.Spec.Assertions (shouldEqual)

import Sudoku.Board

spec :: forall r. Spec r Unit
spec =
  describe "a test test" do
    it "test returns 1" do
      board `shouldEqual` 1
