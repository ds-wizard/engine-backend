module Model.Level.Level where

import Data.Time
import GHC.Generics

data Level = Level
  { _levelLevel :: Int
  , _levelTitle :: String
  , _levelDescription :: Maybe String
  , _levelCreatedAt :: UTCTime
  , _levelUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq Level where
  a == b =
    _levelLevel a == _levelLevel b && _levelTitle a == _levelTitle b && _levelDescription a == _levelDescription b
