module Shared.Util.Number where

import System.Random

generateInt :: Int -> IO Int
generateInt max = do
  number <- randomIO
  return $ number `mod` max

generateIntInRange :: Int -> Int -> IO Int
generateIntInRange min max = randomRIO (min, max)
