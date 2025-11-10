module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireCreateDTO = QuestionnaireCreateDTO
  { name :: String
  , knowledgeModelPackageId :: String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , questionTagUuids :: [U.UUID]
  , documentTemplateId :: Maybe String
  , formatUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
