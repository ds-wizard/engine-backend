module Database.Migration.Migration where

import Context
import qualified
       Database.Migration.KnowledgeModel.KnowledgeModelContainerMigration
       as KMC
import qualified
       Database.Migration.Organization.OrganizationMigration as ORG
import qualified Database.Migration.Package.PackageMigration as PKG
import qualified Database.Migration.User.UserMigration as UM

runMigration context dspConfig = do
  putStrLn "MIGRATION: started"
  ORG.runMigration context dspConfig
  UM.runMigration context dspConfig
  PKG.runMigration context dspConfig
  KMC.runMigration context dspConfig
  putStrLn "MIGRATION: ended"
