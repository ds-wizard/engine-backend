module Wizard.Api.Resource.Document.DocumentContextJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Level.LevelJM ()
import Wizard.Api.Resource.Organization.OrganizationJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON DocumentContextDTO where
  parseJSON = simpleParseJSON "_documentContextDTO"

instance ToJSON DocumentContextDTO where
  toJSON = simpleToJSON "_documentContextDTO"

instance FromJSON DocumentContextConfigDTO where
  parseJSON = simpleParseJSON "_documentContextConfigDTO"

instance ToJSON DocumentContextConfigDTO where
  toJSON = simpleToJSON "_documentContextConfigDTO"
