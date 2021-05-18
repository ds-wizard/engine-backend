module Wizard.Model.Document.DocumentContextJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Level.LevelJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Document.DocumentContext

instance ToJSON DocumentContext where
  toJSON = simpleToJSON "_documentContext"

instance ToJSON DocumentContextConfig where
  toJSON = simpleToJSON "_documentContextConfig"
