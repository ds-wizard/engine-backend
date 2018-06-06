module Database.Migration.Migration where

import Control.Monad.Logger (logInfo)

import qualified Database.Migration.Branch.BranchMigration as BM
import qualified Database.Migration.Migrator.MigratorMigration
       as MM
import qualified
       Database.Migration.Organization.OrganizationMigration as ORG
import qualified Database.Migration.Package.PackageMigration as PKG
import qualified
       Database.Migration.Questionnaire.QuestionnaireMigration as QTN
import qualified Database.Migration.User.UserMigration as UM

runMigration = do
  $(logInfo) "MIGRATION: started"
  ORG.runMigration
  UM.runMigration
  PKG.runMigration
  BM.runMigration
  MM.runMigration
  QTN.runMigration
  $(logInfo) "MIGRATION: ended"
