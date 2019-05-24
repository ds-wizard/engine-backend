module Api.Resource.Event.EventPathDTO where

import qualified Data.UUID as U
import GHC.Generics

data EventPathItemDTO = EventPathItemDTO
  { _eventPathItemDTOPType :: String
  , _eventPathItemDTOUuid :: U.UUID
  } deriving (Show, Eq, Generic)

type EventPathDTO = [EventPathItemDTO]
