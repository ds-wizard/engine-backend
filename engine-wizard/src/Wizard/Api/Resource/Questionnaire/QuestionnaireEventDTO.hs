module Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO

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
    , _setReplyEventDTOValue :: ReplyValueDTO
    }
  deriving (Show, Eq, Generic)

data ClearReplyEventDTO =
  ClearReplyEventDTO
    { _clearReplyEventDTOUuid :: U.UUID
    , _clearReplyEventDTOPath :: String
    }
  deriving (Show, Eq, Generic)

data SetLevelEventDTO =
  SetLevelEventDTO
    { _setLevelEventDTOUuid :: U.UUID
    , _setLevelEventDTOLevel :: Int
    }
  deriving (Show, Eq, Generic)

data SetLabelsEventDTO =
  SetLabelsEventDTO
    { _setLabelsEventDTOUuid :: U.UUID
    , _setLabelsEventDTOPath :: String
    , _setLabelsEventDTOValue :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
