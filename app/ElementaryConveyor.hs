module ElementaryConveyor where

import Prelude hiding (cycle)
import Data.Maybe (fromMaybe)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

type Coord = (Int, Int)


pathCounter :: [String] -> [[Int]]
pathCounter board =
  let
    g = buildGraph board
    f = output g
    grid = coords board
  in
    [ [ fromMaybe (-1) (distance g src f Set.empty)
      | src <- row
      ]
    | row <- grid
    ] 


buildGraph :: [String] -> Map Coord Coord
buildGraph board = Map.fromList
  [ (pos, pointsTo)
  | pos@(i, j) <- concat (coords board)
  , let
      pointsTo = case board !! i !! j of
        'r' -> (i, (j + 1) `mod` rowLen)
        'l' -> (i, (j + (rowLen - 1)) `mod` rowLen)
        'u' -> ((i + (colLen - 1)) `mod` colLen, j)
        'd' -> ((i + 1) `mod` colLen, j)
        'f' -> pos
        _   -> error "bad cell"
      rowLen = length (head board)
      colLen = length board
  ]


output :: Map Coord Coord -> Coord
output = fst . head . filter (uncurry (==)) . Map.toList


distance :: Map Coord Coord -> Coord -> Coord -> Set Coord -> Maybe Int
distance g src dst seen
  | src `Set.member` seen = Nothing
  | src == dst            = Just 0
  | otherwise             = (+ 1) <$> distance g (g Map.! src) dst (Set.insert src seen)


coords :: [[a]] -> [[Coord]]
coords grid = [[(i, j) | j <- [0 .. length (head grid) - 1]] | i <- [0 .. length grid - 1]]

g :: [[Char]]
g = ["rdfrd",
     "uluul"]
