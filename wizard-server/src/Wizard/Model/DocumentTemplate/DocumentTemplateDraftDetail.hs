module Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireSuggestion
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

data DocumentTemplateDraftDetail = DocumentTemplateDraftDetail
  { tId :: String
  , name :: String
  , templateId :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormat]
  , questionnaireUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  , questionnaire :: Maybe QuestionnaireSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
