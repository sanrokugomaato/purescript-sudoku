-- | The collection of functions used in purescript-sudoku code base.
module Sudoku.Util where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (length, (!!))
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

-- | The partial and unsafe version of `fromJust`.
fromJust' :: forall a. Maybe a -> a
fromJust' = unsafePartial fromJust

-- | The `choose` function is used to randomly pick an element from an array.
choose :: forall e. Array ~> Eff (random :: RANDOM | e)
choose [x] = pure x
choose xs = do
  idx <- randomInt 0 (length xs - 1)
  pure $ fromJust' $ xs !! idx
