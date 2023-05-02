module Wizard.Model.Document.DocumentContextJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Document.DocumentContext
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()

instance ToJSON DocumentContext where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextConfig where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextPackage where
  toJSON = genericToJSON jsonOptions
