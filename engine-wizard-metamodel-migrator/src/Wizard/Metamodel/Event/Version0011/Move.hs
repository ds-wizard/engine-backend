module Wizard.Metamodel.Event.Version0011.Move where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0011.Common

-- Shared.Model.Event.KnowledgeModel.MoveEvent
data MoveQuestionEvent =
  MoveQuestionEvent
    { _moveQuestionEventUuid :: U.UUID
    , _moveQuestionEventParentUuid :: U.UUID
    , _moveQuestionEventEntityUuid :: U.UUID
    , _moveQuestionEventTargetUuid :: U.UUID
    , _moveQuestionEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MoveAnswerEvent =
  MoveAnswerEvent
    { _moveAnswerEventUuid :: U.UUID
    , _moveAnswerEventParentUuid :: U.UUID
    , _moveAnswerEventEntityUuid :: U.UUID
    , _moveAnswerEventTargetUuid :: U.UUID
    , _moveAnswerEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MoveChoiceEvent =
  MoveChoiceEvent
    { _moveChoiceEventUuid :: U.UUID
    , _moveChoiceEventParentUuid :: U.UUID
    , _moveChoiceEventEntityUuid :: U.UUID
    , _moveChoiceEventTargetUuid :: U.UUID
    , _moveChoiceEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MoveExpertEvent =
  MoveExpertEvent
    { _moveExpertEventUuid :: U.UUID
    , _moveExpertEventParentUuid :: U.UUID
    , _moveExpertEventEntityUuid :: U.UUID
    , _moveExpertEventTargetUuid :: U.UUID
    , _moveExpertEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MoveReferenceEvent =
  MoveReferenceEvent
    { _moveReferenceEventUuid :: U.UUID
    , _moveReferenceEventParentUuid :: U.UUID
    , _moveReferenceEventEntityUuid :: U.UUID
    , _moveReferenceEventTargetUuid :: U.UUID
    , _moveReferenceEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

-- Shared.Api.Resource.Event.MoveEventJM
instance FromJSON MoveQuestionEvent where
  parseJSON = simpleParseJSON "_moveQuestionEvent"

instance ToJSON MoveQuestionEvent where
  toJSON = simpleToJSON' "_moveQuestionEvent" "eventType"

instance FromJSON MoveAnswerEvent where
  parseJSON = simpleParseJSON "_moveAnswerEvent"

instance ToJSON MoveAnswerEvent where
  toJSON = simpleToJSON' "_moveAnswerEvent" "eventType"

instance FromJSON MoveChoiceEvent where
  parseJSON = simpleParseJSON "_moveChoiceEvent"

instance ToJSON MoveChoiceEvent where
  toJSON = simpleToJSON' "_moveChoiceEvent" "eventType"

instance FromJSON MoveExpertEvent where
  parseJSON = simpleParseJSON "_moveExpertEvent"

instance ToJSON MoveExpertEvent where
  toJSON = simpleToJSON' "_moveExpertEvent" "eventType"

instance FromJSON MoveReferenceEvent where
  parseJSON = simpleParseJSON "_moveReferenceEvent"

instance ToJSON MoveReferenceEvent where
  toJSON = simpleToJSON' "_moveReferenceEvent" "eventType"
