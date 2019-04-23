module Api.Resource.Typehint.TypehintRequestJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Typehint.TypehintRequestDTO

instance FromJSON TypehintRequestDTO where
  parseJSON (Object o) = do
    _typehintRequestDTOPackageId <- o .: "packageId"
    _typehintRequestDTOEvents <- o .: "events"
    _typehintRequestDTOQuestionUuid <- o .: "questionUuid"
    _typehintRequestDTOQ <- o .: "q"
    return TypehintRequestDTO {..}
  parseJSON _ = mzero

instance ToJSON TypehintRequestDTO where
  toJSON TypehintRequestDTO {..} =
    object
      [ "packageId" .= _typehintRequestDTOPackageId
      , "events" .= _typehintRequestDTOEvents
      , "questionUuid" .= _typehintRequestDTOQuestionUuid
      , "q" .= _typehintRequestDTOQ
      ]
