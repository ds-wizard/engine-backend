module Database.Migration.Migration where

import Common.Context
import qualified
       Database.Migration.KnowledgeModel.BranchMigration
       as KMC
import qualified
       Database.Migration.Organization.OrganizationMigration as ORG
import qualified Database.Migration.Package.PackageMigration as PKG
import qualified Database.Migration.User.UserMigration as UM

logState = putStrLn

runMigration context dspConfig = do
  logState "MIGRATION: started"
  ORG.runMigration context dspConfig logState
  UM.runMigration context dspConfig logState
  PKG.runMigration context dspConfig logState
  KMC.runMigration context dspConfig logState
  logState "MIGRATION: ended"
