module Sudoku where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (filter, foldMap, length, range, snoc, updateAt, (!!), (\\))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Partial.Unsafe (unsafePartial)

fromJust' :: forall a. Maybe a -> a
fromJust' = unsafePartial fromJust

data Cell a = Cell Int a

derive instance functorCell :: Functor Cell

newtype Row = Row Int
newtype Col = Col Int
derive instance rowEq :: Eq Row
derive instance colEq :: Eq Col

rowOf :: forall a. Cell a -> Row
rowOf (Cell i _) = Row $ i `div` 9

colOf :: forall a. Cell a -> Col
colOf (Cell i _) = Col $ i `mod` 9

valueOf :: forall a. Cell a -> a
valueOf (Cell _ x) = x

instance showCellMaybeInt :: Show (Cell (Maybe Int)) where
  show (Cell _ (Just x)) = show x
  show (Cell _ Nothing) = "x"

instance showCellInt :: Show (Cell Int) where
  show (Cell _ x) = show x

newtype Board a = Board (Array (Cell a))

derive instance functorBoard :: Functor Board

instance showBoard :: (Show (Cell a)) => Show (Board a) where
  show board = foldMap (Row >>> flip rowCells board >>> foldMap show >>> (<>) "\n") (range 0 8)

emptyBoard :: Board (Maybe Int)
emptyBoard = Board $ map (\i -> Cell i Nothing) (range 0 80)

rowCells :: forall a. Row -> Board a -> Array (Cell a)
rowCells i (Board arr) = filter (rowOf >>> eq i) arr

colCells :: forall a. Col -> Board a -> Array (Cell a)
colCells i (Board arr) = filter (colOf >>> eq i) arr

data Section = Section Int Int -- Section 0 0 ~ Section 2 2

sectionOf :: Row -> Col -> Section
sectionOf (Row r) (Col c) = Section (r `div` 3) (c `div` 3)

contains :: forall a. Section -> Cell a -> Boolean
contains (Section secRow secCol) cell =
  let
    rowIdx = case rowOf cell of Row i -> i
    colIdx = case colOf cell of Col i -> i
  in
   rowIdx >= secRow * 3 && rowIdx < (secRow + 1) * 3 &&
   colIdx >= secCol * 3 && colIdx < (secCol + 1) * 3

sectionCells :: forall a. Section -> Board a -> Array (Cell a)
sectionCells sec (Board arr) = filter (contains sec) arr

rawIdx :: Row -> Col -> Int
rawIdx (Row r) (Col c) = r * 9 + c

valueAt :: forall a. Row -> Col -> Board a -> a
valueAt row col (Board arr) = valueOf $ fromJust' (arr !! rawIdx row col)

choose :: forall e. Array Int -> Eff (random :: RANDOM | e) Int
choose [x] = pure x
choose xs = do
  idx <- randomInt 0 (length xs - 1)
  pure $ fromJust' $ xs !! idx

availableValues :: Row -> Col -> Board (Maybe Int) -> Array Int
availableValues row col board = range 1 9 \\ already
  where
    already = map (fromJust' <<< valueOf)
            $ filter (isJust <<< valueOf)
            $ rowCells row board <> colCells col board <>
              sectionCells (sectionOf row col) board

replaceValue :: forall a. Row -> Col -> a -> Board a -> Board a
replaceValue row col val (Board arr) =
  Board $ fromJust' $ updateAt idx (Cell idx val) arr
  where idx = rawIdx row col

emptyRandomCell :: forall e. Board (Maybe Int) -> Eff (random :: RANDOM | e) (Board (Maybe Int))
emptyRandomCell board = do
  row <- Row <$> randomInt 0 8
  col <- Col <$> randomInt 0 8
  case valueAt row col board of
    Nothing -> emptyRandomCell board -- try again, not smart but works
    Just _ -> pure $ replaceValue row col Nothing board

fullBoard :: forall e. Eff (random :: RANDOM | e) (Board Int)
fullBoard = fromJust' <$> go (Row 0) (Col 0) [] emptyBoard
  where
    go :: Row -> Col -> Array Int -> Board (Maybe Int)-> Eff (random :: RANDOM | e) (Maybe (Board Int))
    go (Row r) (Col 9) _ board = go (Row $ r + 1) (Col 0) [] board
    go (Row 9) _ _ board = pure <<< Just $ fromJust' <$> board
    go row col@(Col c) alreadyTried board =
      case availableValues row col board \\ alreadyTried of
        [] -> pure Nothing
        xs -> do
          curVal <- choose xs
          let curBoard = replaceValue row col (Just curVal) board
          nextBoard <- go row (Col $ c + 1) [] curBoard
          case nextBoard of
            Nothing -> go row col (snoc alreadyTried curVal) board
            justBoard -> pure justBoard

solve :: Board (Maybe Int) -> Array (Board Int)
solve = go (Row 0) (Col 0)
  where
    go :: Row -> Col -> Board (Maybe Int) -> Array (Board Int)
    go (Row r) (Col 9) board = go (Row $ r + 1) (Col 0) board
    go (Row 9) _ board = [fromJust' <$> board]
    go row col@(Col c) board =
      case valueAt row col board of
        Just _ -> go row (Col $ c + 1) board
        Nothing -> flip foldMap (availableValues row col board) \val ->
          go row (Col $ c + 1) $ replaceValue row col (Just val) board

type Difficulty = Int

data Game = Game Difficulty (Board (Maybe Int)) (Board Int)

instance showGame :: Show Game where
  show (Game dify ques answ) =
    "Difficulty: " <> show dify <> "\n\n" <>
    "Board:" <> show ques <> "\n\n" <>
    "Answer:" <> show answ

generateGame :: forall e. Difficulty -> Eff (random :: RANDOM | e) Game
generateGame minDify = do
  answer <- fullBoard
  go 0 answer (Just <$> answer)
  where
    go :: Difficulty -> Board Int -> Board (Maybe Int) -> Eff (random :: RANDOM | e) Game
    go dify answer old = do
      new <- emptyRandomCell old
      case length $ solve new of
        0 -> go dify answer old -- try empty again
        1 -> go (dify + 1) answer new -- try more
        _ -> if dify > minDify
            then pure $ Game dify old answer -- return the game
            else generateGame minDify -- try from scratch! :sad:
