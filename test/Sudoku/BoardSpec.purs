module Sudoku.BoardSpec where

import Prelude

import Test.Spec
import Test.Spec.Assertions (shouldEqual)

import Data.Array (range)

import Sudoku.Board

spec :: forall r. Spec r Unit
spec = do
  let board = Board $ map (\i -> cell (i `div` 9) (i `mod` 9) unit) (range 0 80)

  describe "createBoard" do
    it "create board" do
      createBoard (\_ -> unit) `shouldEqual` board

  describe "nthRow" do
    it "gets the nth row" do
      nthRow board 3 `shouldEqual` map (\i -> cell 3 i unit) (range 0 8)

  describe "nthCol" do
    it "gets the nth col" do
      nthCol board 5 `shouldEqual` map (\i -> cell i 5 unit) (range 0 8)
