module Example.Simple where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM)
import Data.Tuple (Tuple(..))
import Sudoku (generateGame, solve)

main :: Eff (random :: RANDOM, console :: CONSOLE) Unit
main = do
  let minDify = 45
  log $ "Minimum difficulty level: " <> show minDify <> "\n"

  Tuple dify board <- generateGame minDify
  log $ "Difficulty level: " <> show dify <> "\nGame board"
  logShow board

  log "\nSolution:"
  logShow $ solve board
