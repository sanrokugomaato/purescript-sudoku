module Test.Sudoku.UtilSpec where

import Prelude

import Test.Unit (suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(..), (===), (<?>))

import Sudoku.Util

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (length)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))

spec = suite "Test.Sudoku.UtilSpec" do
  suite "fromJust'" do
    test "retrieve value from Just" do
      quickCheck \ x ->
        (x :: Int) === fromJust' (Just x)

  suite "choose" do
    test "choose value from array" do
      quickCheck \ xs ->
        if length xs == 0
        then Success
        else unsafePerformEff do
          x <- choose (xs :: Array Int)
          pure $ x `elem` xs <?> "should contain"
