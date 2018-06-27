module Database.Migration.Migration where

import Control.Monad.Logger (logInfo)

import qualified
       Database.Migration.BookReference.BookReferenceMigration as BR
import qualified Database.Migration.Branch.BranchMigration as B
import qualified Database.Migration.Migrator.MigratorMigration as M
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
  B.runMigration
  M.runMigration
  QTN.runMigration
  BR.runMigration
  $(logInfo) "MIGRATION: ended"
