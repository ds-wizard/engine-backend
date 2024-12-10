module Wizard.Api.Resource.Branch.BranchDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchStateJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()

instance FromJSON BranchDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BranchDetailDTO where
  toJSON = genericToJSON jsonOptions
