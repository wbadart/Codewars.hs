{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Sudoku where

import Data.List (minimumBy, transpose)
import Data.Ord (comparing)
import Data.Set (Set, (\\))
import qualified Data.Set as Set


-- Main API
-- ==========

sudoku :: [[Int]] -> [[Int]]
sudoku = boardRows . head . solve (Set.fromList [1 .. 9]) 0 . denormalize


-- Data structures
-- ==========

data Board a = Board
  { boardZones :: Zones a
  , boardRows  :: [[a]]
  }
  deriving Show

data Zones a = Zones
  { zoneRows :: [Set a]
  , zoneCols :: [Set a]
  , zoneSqs  :: [Set a]
  }
  deriving Show

type Pos = (Int, Int)

denormalize :: [[Int]] -> Board Int
denormalize board = Board
  { boardZones = Zones
    { zoneRows = Set.filter (/= 0) . Set.fromList <$> board
    , zoneCols = Set.filter (/= 0) . Set.fromList <$> transpose board
    , zoneSqs  = Set.filter (/= 0) . Set.fromList <$> squares board
    }
  , boardRows = board
  }

squares :: [[a]] -> [[a]]
squares board =
  let
    boardSize = length board
    numSqs = floor @Double (sqrt $ fromIntegral boardSize)
  in
    [ square board numSqs i j
    | i <- [0 .. numSqs - 1]
    , j <- [0 .. numSqs - 1]
    ]

square :: [[a]] -> Int -> Int -> Int -> [a]
square board numSqs rowIndex colIndex =
  let
    rowMin = numSqs * rowIndex
    colMin = numSqs * colIndex
  in
    [ board !! i !! j
    | i <- [rowMin .. rowMin + numSqs - 1]
    , j <- [colMin .. colMin + numSqs - 1]
    ]


-- Algorithm
-- ==========

solve :: (Show a, Ord a) => Set a -> a -> Board a -> [Board a]
solve alphabet sentinel board@Board{boardRows}
  | done sentinel board = pure board
  | otherwise =
      case emptyPositions of
        [] -> []
        _  -> do
          -- pick the empty position with the fewest candidates
          let bestPos = minimumBy (comparing (length . candidates alphabet board)) emptyPositions
          candidate <- candidates alphabet board bestPos
          let board' = set bestPos candidate board
          if done sentinel board'
            then pure board'
            else solve alphabet sentinel board'
      where
        emptyPositions = filter (\(i, j) -> boardRows !! i !! j == sentinel) (coords board)

coords :: Board a -> [Pos]
coords Board{boardRows=board} =
  let boardSize = length board
  in [(i, j) | i <- [0 .. boardSize - 1], j <- [0 .. boardSize - 1]]

candidates :: Ord a => Set a -> Board a -> Pos -> [a]
candidates alphabet Board{boardZones, boardRows} pos@(rowIndex, colIndex) =
  Set.toList
    $ alphabet
    \\ Set.unions
      [ zoneRows boardZones !! rowIndex
      , zoneCols boardZones !!  colIndex
      , zoneSqs boardZones !! sqIndex (length boardRows) pos
      ]


zone :: Board a -> Pos -> [Set a]
zone Board{boardZones, boardRows} pos@(rowIndex, colIndex) =
  [ zoneRows boardZones !! rowIndex
  , zoneCols boardZones !! colIndex
  , zoneSqs boardZones !! sqIndex (length boardRows) pos
  ]


set :: Ord a => Pos -> a -> Board a -> Board a
set pos@(rowIndex, colIndex) a Board{boardZones, boardRows} = Board
  { boardZones = Zones
    { zoneRows = updateList rowIndex (Set.insert a) (zoneRows boardZones)
    , zoneCols = updateList colIndex (Set.insert a) (zoneCols boardZones)
    , zoneSqs  = updateList (sqIndex (length boardRows) pos) (Set.insert a) (zoneSqs boardZones)
    }
  , boardRows = updateList rowIndex (updateList colIndex (const a)) boardRows
  }


done :: Eq a => a -> Board a -> Bool
done sentinel Board{boardRows} = sentinel `notElem` concat boardRows


-- Helpers
-- ==========

sqIndex :: Int -> Pos -> Int
sqIndex boardSize (rowIndex, colIndex) =
  let
    numSqs = floor @Double (sqrt $ fromIntegral boardSize)
    i = rowIndex `div` numSqs
    j = colIndex `div` numSqs
  in
    numSqs * i + j


updateList :: Int -> (a -> a) -> [a] -> [a]
updateList i f xs =
  let
    (left, right) = splitAt i xs
    x = xs !! i
  in
    left <> (f x : drop 1 right)
