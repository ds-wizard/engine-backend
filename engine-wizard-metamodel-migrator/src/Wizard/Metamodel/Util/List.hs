module Wizard.Metamodel.Util.List
  ( foldEither
  ) where

import Data.Either (partitionEithers)

foldEither :: [Either l r] -> Either l [r]
foldEither eitherList =
  case partitionEithers eitherList of
    (l:_, rs) -> Left l
    (_, rs) -> Right rs
