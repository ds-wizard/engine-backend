module Wizard.Api.Resource.Document.DocumentContextJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Level.LevelJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON DocumentContextDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DocumentContextDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON DocumentContextConfigDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DocumentContextConfigDTO where
  toJSON = genericToJSON simpleOptions
