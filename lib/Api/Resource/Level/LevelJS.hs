module Api.Resource.Level.LevelJS where

import Control.Monad
import Data.Aeson

import Api.Resource.Level.LevelDTO

instance FromJSON LevelDTO where
  parseJSON (Object o) = do
    _levelDTOLevel <- o .: "level"
    _levelDTOTitle <- o .: "title"
    _levelDTODescription <- o .: "description"
    _levelDTOCreatedAt <- o .: "createdAt"
    _levelDTOUpdatedAt <- o .: "updatedAt"
    return LevelDTO {..}
  parseJSON _ = mzero

instance ToJSON LevelDTO where
  toJSON LevelDTO {..} =
    object
      [ "level" .= _levelDTOLevel
      , "title" .= _levelDTOTitle
      , "description" .= _levelDTODescription
      , "createdAt" .= _levelDTOCreatedAt
      , "updatedAt" .= _levelDTOUpdatedAt
      ]
