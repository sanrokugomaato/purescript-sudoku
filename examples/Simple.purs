module Example.Simple where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array ((!!))
import Sudoku (generateGame, solve)
import Sudoku.Types (Game(..))
import Sudoku.Util (fromJust')

main :: Eff (random :: RANDOM, console :: CONSOLE) Unit
main = do
  let minDify = 45
  log $ "Minimum difficulty level: " <> show minDify <> "\n"

  game@(Game dify board answer) <- generateGame minDify
  logShow game

  log "Solution:"
  let solution = fromJust' $ solve board !! 0 -- unique, so get the first answer
  logShow solution
  log $ "Result: " <>
    if solution == answer
    then "ok!"
    else "meh."
