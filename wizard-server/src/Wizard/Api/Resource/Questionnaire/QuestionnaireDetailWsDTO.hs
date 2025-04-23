module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO

data QuestionnaireDetailWsDTO = QuestionnaireDetailWsDTO
  { name :: String
  , description :: Maybe String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , projectTags :: [String]
  , permissions :: [QuestionnairePermDTO]
  , documentTemplateId :: Maybe String
  , documentTemplate :: Maybe DocumentTemplateDTO
  , formatUuid :: Maybe U.UUID
  , format :: Maybe DocumentTemplateFormatDTO
  , isTemplate :: Bool
  , labels :: M.Map String [U.UUID]
  , unresolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  , resolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  }
  deriving (Show, Eq, Generic)
