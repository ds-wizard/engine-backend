module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireDetailDTO = QuestionnaireDetailDTO
  { uuid :: U.UUID
  , name :: String
  , sharing :: QuestionnaireSharing
  , visibility :: QuestionnaireVisibility
  , knowledgeModelPackageId :: String
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePermDTO]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
