module Model.Event.EventPath where

import qualified Data.UUID as U
import GHC.Generics

_EVENT_PATH_ITEM__KM = "km"

_EVENT_PATH_ITEM__CHAPTER = "chapter"

_EVENT_PATH_ITEM__QUESTION = "question"

_EVENT_PATH_ITEM__ANSWER = "answer"

_EVENT_PATH_ITEM__EXPERT = "expert"

_EVENT_PATH_ITEM__REFERENCE = "reference"

data EventPathItem = EventPathItem
  { _eventPathItemPType :: String
  , _eventPathItemUuid :: U.UUID
  } deriving (Show, Eq, Generic)

type EventPath = [EventPathItem]

showEventPathShort path =
  unwords $
  (\pathItem -> "[" ++ (_eventPathItemPType pathItem) ++ ": " ++ U.toString (_eventPathItemUuid pathItem) ++ "]") <$>
  path
