module Wizard.Model.Questionnaire.QuestionnaireDetailPreview where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

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
  , format :: Maybe DocumentTemplateFormatSimple
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
