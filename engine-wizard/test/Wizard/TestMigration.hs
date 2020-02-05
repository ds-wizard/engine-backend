module Wizard.TestMigration where

import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Database.DAO.Metric.MetricDAO
import qualified Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO as KM_MigratorDAO
import qualified Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO as QTN_MigratorDAO
import Wizard.Database.DAO.Organization.OrganizationDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.Organization.Data.Organizations
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.User.Data.Users

import Wizard.Specs.Common

resetDB appContext = do
  runInContext deleteUsers appContext
  runInContext (insertUser userAlbert) appContext
  runInContext deleteOrganizations appContext
  runInContext (insertOrganization org1) appContext
  runInContext deleteBranches appContext
  runInContext deleteMetrics appContext
  runInContext deleteActionKeys appContext
  runInContext deleteDocuments appContext
  runInContext deleteFeedbacks appContext
  runInContext deleteQuestionnaires appContext
  runInContext KM_MigratorDAO.deleteMigratorStates appContext
  runInContext QTN_MigratorDAO.deleteMigratorStates appContext
  runInContext deletePackages appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  return ()
