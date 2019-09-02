module Api.Resource.Document.DocumentContextJM where

import Data.Aeson

import Api.Resource.Document.DocumentContextDTO
import Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Api.Resource.Level.LevelJM ()
import Api.Resource.Organization.OrganizationJM ()
import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Api.Resource.Report.ReportJM ()
import Api.Resource.User.UserJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON DocumentContextDTO where
  parseJSON = simpleParseJSON "_documentContextDTO"

instance ToJSON DocumentContextDTO where
  toJSON = simpleToJSON "_documentContextDTO"

instance FromJSON DocumentContextConfigDTO where
  parseJSON = simpleParseJSON "_documentConfigDTO"

instance ToJSON DocumentContextConfigDTO where
  toJSON = simpleToJSON "_documentConfigDTO"
