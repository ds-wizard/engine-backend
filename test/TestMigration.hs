module TestMigration where

import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Feedback.FeedbackDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.DAO.Package.PackageDAO
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
  runInContext (deleteMigratorStates) appContext
  runInContext (deleteActionKeys) appContext
  runInContext (deleteFeedbacks) appContext
  runInContext (deleteQuestionnaires) appContext
  runInContext (deletePackages) appContext
  runInContext (insertPackage baseElixir0PackageDto) appContext
  runInContext (insertPackage baseElixirPackageDto) appContext
  runInContext (insertPackage elixirNlPackageDto) appContext
  runInContext (insertPackage elixirNlPackage2Dto) appContext
  return ()
