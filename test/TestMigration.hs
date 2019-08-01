module TestMigration where

import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Feedback.FeedbackDAO
import Database.DAO.Metric.MetricDAO
import qualified Database.DAO.Migration.KnowledgeModel.MigratorDAO
       as KM_MigratorDAO
import qualified Database.DAO.Migration.Questionnaire.MigratorDAO
       as QTN_MigratorDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.DAO.User.UserDAO
import Database.Migration.Development.Organization.Data.Organizations
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.User.Data.Users

import Specs.Common

resetDB appContext = do
  runInContext (deleteUsers) appContext
  runInContext (insertUser userAlbert) appContext
  runInContext (deleteOrganizations) appContext
  runInContext (insertOrganization org1) appContext
  runInContext (deleteBranches) appContext
  runInContext (deleteMetrics) appContext
  runInContext (deleteActionKeys) appContext
  runInContext (deleteFeedbacks) appContext
  runInContext (deleteQuestionnaires) appContext
  runInContext (KM_MigratorDAO.deleteMigratorStates) appContext
  runInContext (QTN_MigratorDAO.deleteMigratorStates) appContext
  runInContext (deletePackages) appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  return ()
