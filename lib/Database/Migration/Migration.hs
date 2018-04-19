module Database.Migration.Migration where

import Control.Monad.Logger (logInfo)

import qualified Database.Migration.Branch.BranchMigration as BM
import qualified Database.Migration.Migrator.MigratorMigration
       as MM
import qualified
       Database.Migration.Organization.OrganizationMigration as ORG
import qualified Database.Migration.Package.PackageMigration as PKG
import qualified Database.Migration.User.UserMigration as UM
import Model.Context.AppContext

runMigration appContext = do
  $(logInfo) "MIGRATION: started"
  ORG.runMigration appContext
  UM.runMigration appContext
  PKG.runMigration appContext
  BM.runMigration appContext
  MM.runMigration appContext
  $(logInfo) "MIGRATION: ended"
