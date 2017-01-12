module Sudoku.Util where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (length, (!!))
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

fromJust' :: forall a. Maybe a -> a
fromJust' = unsafePartial fromJust

choose :: forall e. Array Int -> Eff (random :: RANDOM | e) Int
choose [x] = pure x
choose xs = do
  idx <- randomInt 0 (length xs - 1)
  pure $ fromJust' $ xs !! idx
