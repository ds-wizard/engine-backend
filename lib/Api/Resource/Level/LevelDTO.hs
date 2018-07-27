module Api.Resource.Level.LevelDTO where

import Data.Time

data LevelDTO = LevelDTO
  { _levelDTOLevel :: Int
  , _levelDTOTitle :: String
  , _levelDTODescription :: Maybe String
  , _levelDTOCreatedAt :: UTCTime
  , _levelDTOUpdatedAt :: UTCTime
  } deriving (Show)

instance Eq LevelDTO where
  a == b =
    _levelDTOLevel a == _levelDTOLevel b &&
    _levelDTOTitle a == _levelDTOTitle b && _levelDTODescription a == _levelDTODescription b
