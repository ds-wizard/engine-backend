module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireEventChangeDTO
  = SetReplyEventChangeDTO' SetReplyEventChangeDTO
  | ClearReplyEventChangeDTO' ClearReplyEventChangeDTO
  | SetLevelEventChangeDTO' SetLevelEventChangeDTO
  | SetLabelsEventChangeDTO' SetLabelsEventChangeDTO
  deriving (Show, Eq, Generic)

data SetReplyEventChangeDTO =
  SetReplyEventChangeDTO
    { _setReplyEventChangeDTOUuid :: U.UUID
    , _setReplyEventChangeDTOPath :: String
    , _setReplyEventChangeDTOValue :: ReplyValue
    }
  deriving (Show, Eq, Generic)

data ClearReplyEventChangeDTO =
  ClearReplyEventChangeDTO
    { _clearReplyEventChangeDTOUuid :: U.UUID
    , _clearReplyEventChangeDTOPath :: String
    }
  deriving (Show, Eq, Generic)

data SetLevelEventChangeDTO =
  SetLevelEventChangeDTO
    { _setLevelEventChangeDTOUuid :: U.UUID
    , _setLevelEventChangeDTOLevel :: Int
    }
  deriving (Show, Eq, Generic)

data SetLabelsEventChangeDTO =
  SetLabelsEventChangeDTO
    { _setLabelsEventChangeDTOUuid :: U.UUID
    , _setLabelsEventChangeDTOPath :: String
    , _setLabelsEventChangeDTOValue :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
