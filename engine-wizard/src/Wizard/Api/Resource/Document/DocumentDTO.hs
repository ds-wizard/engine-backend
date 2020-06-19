module Wizard.Api.Resource.Document.DocumentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Template.Template
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Model.Document.Document

data DocumentDTO =
  DocumentDTO
    { _documentDTOUuid :: U.UUID
    , _documentDTOName :: String
    , _documentDTOState :: DocumentState
    , _documentDTOQuestionnaire :: Maybe QuestionnaireDTO
    , _documentDTOTemplate :: Template
    , _documentDTOFormatUuid :: U.UUID
    , _documentDTOOwnerUuid :: U.UUID
    , _documentDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
