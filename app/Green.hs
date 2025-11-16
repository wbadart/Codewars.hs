module Green where

import Prelude hiding (exp)
import Data.List (isSuffixOf, unfoldr)
import GHC.Integer (divInteger)


green :: Int -> Integer
green n = filter (isGreen 10) [1 ..] !! (n - 1)


isGreen :: Int -> Integer -> Bool
isGreen base n = digits base n `isSuffixOf` digits base (n ^ 2)


digits :: Int -> Integer -> [Integer]
digits base n =
  let power0 = floor (logBase (fromIntegral base) (fromIntegral n) :: Double) :: Integer
  in unfoldr go (power0, n)
  where
    go :: (Integer, Integer) -> Maybe (Integer, (Integer, Integer))
    go (power, n') =
      if power >= 0
        then 
          let msd = mostSignificantDigit base n'
          in Just (msd, (power - 1, n' - msd * fromIntegral base ^ fromIntegral power))
        else Nothing


mostSignificantDigit :: Int -> Integer -> Integer
mostSignificantDigit base n
  | n == 0 = 0
  | otherwise =
      let power = floor (logBase (fromIntegral base) (fromIntegral n) :: Double) :: Integer
      in n `divInteger` (fromIntegral base ^ power)
