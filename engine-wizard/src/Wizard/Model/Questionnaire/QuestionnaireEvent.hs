module Wizard.Model.Questionnaire.QuestionnaireEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireEvent
  = SetReplyEvent' SetReplyEvent
  | ClearReplyEvent' ClearReplyEvent
  | SetLevelEvent' SetLevelEvent
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

data SetLevelEvent =
  SetLevelEvent
    { _setLevelEventUuid :: U.UUID
    , _setLevelEventLevel :: Int
    , _setLevelEventCreatedBy :: Maybe U.UUID
    , _setLevelEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetLevelEvent where
  a == b =
    _setLevelEventUuid a == _setLevelEventUuid b &&
    _setLevelEventLevel a == _setLevelEventLevel b && _setLevelEventCreatedBy a == _setLevelEventCreatedBy b

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
