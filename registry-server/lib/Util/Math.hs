module Util.Math where

weightAverage :: Fractional a => [(a, a)] -> a
weightAverage [] = 0
weightAverage xs = (multiples xs) / (sumOfWeights xs)
  where
    multiples = foldl (\acc (m, w) -> acc + m * w) 0
    sumOfWeights = foldl (\acc (_, w) -> acc + w) 0
