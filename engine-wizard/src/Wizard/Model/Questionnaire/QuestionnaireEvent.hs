module Wizard.Model.Questionnaire.QuestionnaireEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireEventBundle =
  QuestionnaireEventBundle
    { _questionnaireEventBundleEvents :: [QuestionnaireEvent]
    }
  deriving (Show, Generic)

data QuestionnaireEvent
  = SetReplyEvent' SetReplyEvent
  | ClearReplyEvent' ClearReplyEvent
  | SetPhaseEvent' SetPhaseEvent
  | SetLabelsEvent' SetLabelsEvent
  deriving (Show, Eq, Generic)

data SetReplyEvent =
  SetReplyEvent
    { _setReplyEventUuid :: U.UUID
    , _setReplyEventPath :: String
    , _setReplyEventValue :: ReplyValue
    , _setReplyEventCreatedBy :: Maybe U.UUID
    , _setReplyEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetReplyEvent where
  a == b =
    _setReplyEventUuid a == _setReplyEventUuid b &&
    _setReplyEventPath a == _setReplyEventPath b &&
    _setReplyEventValue a == _setReplyEventValue b && _setReplyEventCreatedBy a == _setReplyEventCreatedBy b

data ClearReplyEvent =
  ClearReplyEvent
    { _clearReplyEventUuid :: U.UUID
    , _clearReplyEventPath :: String
    , _clearReplyEventCreatedBy :: Maybe U.UUID
    , _clearReplyEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ClearReplyEvent where
  a == b =
    _clearReplyEventUuid a == _clearReplyEventUuid b &&
    _clearReplyEventPath a == _clearReplyEventPath b && _clearReplyEventCreatedBy a == _clearReplyEventCreatedBy b

data SetPhaseEvent =
  SetPhaseEvent
    { _setPhaseEventUuid :: U.UUID
    , _setPhaseEventPhaseUuid :: Maybe U.UUID
    , _setPhaseEventCreatedBy :: Maybe U.UUID
    , _setPhaseEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetPhaseEvent where
  a == b =
    _setPhaseEventUuid a == _setPhaseEventUuid b &&
    _setPhaseEventPhaseUuid a == _setPhaseEventPhaseUuid b && _setPhaseEventCreatedBy a == _setPhaseEventCreatedBy b

data SetLabelsEvent =
  SetLabelsEvent
    { _setLabelsEventUuid :: U.UUID
    , _setLabelsEventPath :: String
    , _setLabelsEventValue :: [U.UUID]
    , _setLabelsEventCreatedBy :: Maybe U.UUID
    , _setLabelsEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetLabelsEvent where
  a == b =
    _setLabelsEventUuid a == _setLabelsEventUuid b &&
    _setLabelsEventPath a == _setLabelsEventPath b &&
    _setLabelsEventValue a == _setLabelsEventValue b && _setLabelsEventCreatedBy a == _setLabelsEventCreatedBy b
