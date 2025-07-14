module Shared.Common.Model.Common.SemVer2Tuple where

import GHC.Generics

data SemVer2Tuple = SemVer2Tuple
  { major :: Int
  , minor :: Int
  }
  deriving (Eq, Generic, Read)

instance Show SemVer2Tuple where
  show (SemVer2Tuple major minor) = show major ++ "." ++ show minor
