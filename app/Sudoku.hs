module Sudoku where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (groupBy, sortOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map


-- Main API
-- ==========

sudoku :: [[Int]] -> [[Int]]
sudoku = toInput . head . solve . fromInput


-- Config
-- ==========

squareLength :: Int
squareLength = 3

boardLength :: Int
boardLength = squareLength * squareLength


-- Data
-- ==========

type Board a = Map Coord a
type Coord = (Int, Int)

fromInput :: [[a]] -> Board a
fromInput board = Map.fromList [(pos, board !! i !! j) | pos@(i, j) <- coords]

toInput :: Board a -> [[a]]
toInput board =
  Map.toList board
  & groupBy (\((rowA, _), _) ((rowB, _), _) -> rowA == rowB)
  <&> sortOn (\((_, col), _) -> col)
  <&> fmap snd


-- Algorithm
-- ==========

solve :: Board Int -> [Board Int]
solve board = do
  pos <- coords & filter (\pos -> board ! pos == 0)
  candidate <- candidates board pos
  let
    board' = set board pos candidate
  if done board'
    then pure board'
    else solve board'


candidates :: Board Int -> Coord -> [Int]
candidates board pos@(rowIndex, colIndex) = filter rules alphabet
  where
    rules :: Int -> Bool
    rules val = uniqRow val && uniqCol val && uniqSquare val

    uniqRow, uniqCol, uniqSquare :: Int -> Bool
    uniqRow    val = val `notElem` row rowIndex board
    uniqCol    val = val `notElem` col colIndex board
    uniqSquare val = val `notElem` square pos board

    alphabet :: [Int]
    alphabet = [1 .. boardLength]


done :: Board Int -> Bool
done board = 0 `notElem` Map.elems board


-- Helpers
-- ==========

coords :: [Coord]
coords = [(i, j) | i <- [0 .. boardLength - 1], j <- [0 .. boardLength -1]]

set :: Board a -> Coord -> a -> Board a
set board pos val = Map.insert pos val board

row, col :: Int -> Board a -> [a]
row i = Map.elems . Map.filterWithKey (\(rowIndex, _) _ -> rowIndex == i)
col j = Map.elems . Map.filterWithKey (\(_, colIndex) _ -> colIndex == j)

square :: Coord -> Board a -> [a]
square (rowIndex, colIndex) board =
  let
    rowMin = squareLength * (rowIndex `div` squareLength)
    rowMax = rowMin + squareLength - 1
    colMin = squareLength * (colIndex `div` squareLength)
    colMax = colMin + squareLength - 1
  in [board ! (i, j) | i <- [rowMin .. rowMax], j <- [colMin .. colMax]]
