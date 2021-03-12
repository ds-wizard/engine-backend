module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireEventDTO
  = SetReplyEventDTO' SetReplyEventDTO
  | ClearReplyEventDTO' ClearReplyEventDTO
  | SetLevelEventDTO' SetLevelEventDTO
  | SetLabelsEventDTO' SetLabelsEventDTO
  deriving (Show, Eq, Generic)

data SetReplyEventDTO =
  SetReplyEventDTO
    { _setReplyEventDTOUuid :: U.UUID
    , _setReplyEventDTOPath :: String
    , _setReplyEventDTOValue :: ReplyValue
    , _setReplyEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _setReplyEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetReplyEventDTO where
  a == b =
    _setReplyEventDTOUuid a == _setReplyEventDTOUuid b &&
    _setReplyEventDTOPath a == _setReplyEventDTOPath b &&
    _setReplyEventDTOValue a == _setReplyEventDTOValue b && _setReplyEventDTOCreatedBy a == _setReplyEventDTOCreatedBy b

data ClearReplyEventDTO =
  ClearReplyEventDTO
    { _clearReplyEventDTOUuid :: U.UUID
    , _clearReplyEventDTOPath :: String
    , _clearReplyEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _clearReplyEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ClearReplyEventDTO where
  a == b =
    _clearReplyEventDTOUuid a == _clearReplyEventDTOUuid b &&
    _clearReplyEventDTOPath a == _clearReplyEventDTOPath b &&
    _clearReplyEventDTOCreatedBy a == _clearReplyEventDTOCreatedBy b

data SetLevelEventDTO =
  SetLevelEventDTO
    { _setLevelEventDTOUuid :: U.UUID
    , _setLevelEventDTOLevel :: Int
    , _setLevelEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _setLevelEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetLevelEventDTO where
  a == b =
    _setLevelEventDTOUuid a == _setLevelEventDTOUuid b &&
    _setLevelEventDTOLevel a == _setLevelEventDTOLevel b && _setLevelEventDTOCreatedBy a == _setLevelEventDTOCreatedBy b

data SetLabelsEventDTO =
  SetLabelsEventDTO
    { _setLabelsEventDTOUuid :: U.UUID
    , _setLabelsEventDTOPath :: String
    , _setLabelsEventDTOValue :: [U.UUID]
    , _setLabelsEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _setLabelsEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetLabelsEventDTO where
  a == b =
    _setLabelsEventDTOUuid a == _setLabelsEventDTOUuid b &&
    _setLabelsEventDTOPath a == _setLabelsEventDTOPath b &&
    _setLabelsEventDTOValue a == _setLabelsEventDTOValue b &&
    _setLabelsEventDTOCreatedBy a == _setLabelsEventDTOCreatedBy b
