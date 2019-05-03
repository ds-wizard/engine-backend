module Database.Migration.Development.DataManagementPlan.Data.DataManagementPlans where

import Control.Lens ((^.))
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import Database.Migration.Development.FilledKnowledgeModel.Data.FilledKnowledgeModels
import Database.Migration.Development.Level.Data.Levels
import Database.Migration.Development.Metric.Data.Metrics
import Database.Migration.Development.Organization.Data.Organizations
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import Database.Migration.Development.Report.Data.Reports
import Database.Migration.Development.User.Data.Users
import LensesConfig
import Model.DataManagementPlan.DataManagementPlan
import Service.Package.PackageMapper

dmp1 :: DataManagementPlan
dmp1 =
  DataManagementPlan
  { _dataManagementPlanUuid = fromJust (U.fromString "d87941ae-7725-4d22-b5c7-45dabc125199")
  , _dataManagementPlanQuestionnaireUuid = U.toString $ questionnaire1 ^. uuid
  , _dataManagementPlanLevel = questionnaire1 ^. level
  , _dataManagementPlanFilledKnowledgeModel = fKm1WithQ4
  , _dataManagementPlanMetrics = [metricF, metricA, metricI, metricR, metricG, metricO]
  , _dataManagementPlanLevels = [level1, level2, level3]
  , _dataManagementPlanReport = report1
  , _dataManagementPlanPackage = packageWithEventsToPackage germanyPackage
  , _dataManagementPlanOrganization = org1
  , _dataManagementPlanCreatedBy = Just userAlbert
  , _dataManagementPlanCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _dataManagementPlanUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }
