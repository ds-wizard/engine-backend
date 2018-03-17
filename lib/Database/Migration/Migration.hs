module Database.Migration.Migration where

import Common.Context
import qualified Database.Migration.Branch.BranchMigration as BM
import qualified Database.Migration.Migrator.MigratorMigration
       as MM
import qualified
       Database.Migration.Organization.OrganizationMigration as ORG
import qualified Database.Migration.Package.PackageMigration as PKG
import qualified Database.Migration.User.UserMigration as UM

logState = putStrLn

runMigration context dswConfig = do
  logState "MIGRATION: started"
  ORG.runMigration context dswConfig logState
  UM.runMigration context dswConfig logState
  PKG.runMigration context dswConfig logState
  BM.runMigration context dswConfig logState
  MM.runMigration context dswConfig logState
  logState "MIGRATION: ended"
