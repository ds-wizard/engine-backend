module Api.Resource.Event.EventPathDTO where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U

data EventPathItemDTO = EventPathItemDTO
  { _eventPathItemDTOPType :: String
  , _eventPathItemDTOUuid :: U.UUID
  } deriving (Show, Eq)

type EventPathDTO = [EventPathItemDTO]

instance FromJSON EventPathItemDTO where
  parseJSON (Object o) = do
    _eventPathItemDTOPType <- o .: "type"
    _eventPathItemDTOUuid <- o .: "uuid"
    return EventPathItemDTO {..}
  parseJSON _ = mzero

instance ToJSON EventPathItemDTO where
  toJSON EventPathItemDTO {..} = object ["type" .= _eventPathItemDTOPType, "uuid" .= _eventPathItemDTOUuid]
