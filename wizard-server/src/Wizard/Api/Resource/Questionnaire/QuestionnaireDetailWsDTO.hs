module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO where

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
  }
  deriving (Show, Eq, Generic)
