module Shared.Util.Math where

weightAverage :: (Eq a, Fractional a) => [(a, a)] -> a
weightAverage [] = 0
weightAverage xs =
  case sumOfWeights xs of
    0 -> 0
    _ -> multiples xs / sumOfWeights xs
  where
    multiples = foldl (\acc (m, w) -> acc + m * w) 0
    sumOfWeights = foldl (\acc (_, w) -> acc + w) 0
