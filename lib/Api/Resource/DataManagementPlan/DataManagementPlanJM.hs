module Api.Resource.DataManagementPlan.DataManagementPlanJM where

import Data.Aeson

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelJM ()
import Api.Resource.Level.LevelJM ()
import Api.Resource.Organization.OrganizationJM ()
import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.Report.ReportJM ()
import Api.Resource.User.UserJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON DataManagementPlanDTO where
  parseJSON = simpleParseJSON "_dataManagementPlanDTO"

instance ToJSON DataManagementPlanDTO where
  toJSON = simpleToJSON "_dataManagementPlanDTO"

instance FromJSON DataManagementPlanConfigDTO where
  parseJSON = simpleParseJSON "_dataManagementPlanConfigDTO"

instance ToJSON DataManagementPlanConfigDTO where
  toJSON = simpleToJSON "_dataManagementPlanConfigDTO"
