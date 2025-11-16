module ElementaryConveyor where

import Prelude hiding (cycle, succ, pred)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)

-- Solution
-- ==========

pathCounter :: [String] -> [[Int]]
pathCounter board =
  let
    succ = buildGraph board
    pred = invert succ
    dist = bfsDistances pred (output succ)
  in
    [ [ fromMaybe (-1) (Map.lookup src dist)
      | src <- row
      ]
    | row <- coords board
    ]


-- Intermediate data structures
-- ==========

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


invert :: Map Coord Coord -> Map Coord [Coord]
invert g =
  let
    initMap = Map.fromList [ (k, []) | k <- Map.keys g ]
    addPred m (src, dst) = Map.insertWith (<>) dst [src] m
  in
    foldl' addPred initMap (Map.toList g)


-- Helpers
-- ==========

bfsDistances :: Map Coord [Coord] -> Coord -> Map Coord Int
bfsDistances pred start =
  let dist0 = Map.singleton start 0
  in bfs [start] 0 dist0
  where
    bfs :: [Coord] -> Int -> Map Coord Int -> Map Coord Int
    bfs [] _ dist = dist
    bfs frontier d dist =
      let
        -- expand one layer: for each node in frontier, consider its predecessors
        expand (distAcc, nextFront) node =
          foldl' (processPred node) (distAcc, nextFront) (Map.findWithDefault [] node pred)

        processPred _ (dAcc, nf) p =
          if Map.member p dAcc
            then (dAcc, nf)
            else (Map.insert p (d + 1) dAcc, p : nf)

        (dist', nextFrontier) = foldl' expand (dist, []) frontier
      in
        if null nextFrontier
          then dist'
          else bfs (reverse nextFrontier) (d + 1) dist'


coords :: [[a]] -> [[Coord]]
coords grid = [[(i, j) | j <- [0 .. length (head grid) - 1]] | i <- [0 .. length grid - 1]]


output :: Map Coord Coord -> Coord
output = fst . head . filter (uncurry (==)) . Map.toList
