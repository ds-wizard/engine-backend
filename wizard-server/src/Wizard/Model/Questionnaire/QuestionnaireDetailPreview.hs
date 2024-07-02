module Wizard.Model.Questionnaire.QuestionnaireDetailPreview where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO

data QuestionnaireDetailPreview = QuestionnaireDetailPreview
  { uuid :: U.UUID
  , name :: String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , packageId :: String
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePermDTO]
  , documentTemplateId :: Maybe String
  , format :: Maybe DocumentTemplateFormatDTO
  }
  deriving (Show, Eq, Generic)
