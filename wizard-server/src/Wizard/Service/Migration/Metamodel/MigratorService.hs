module Wizard.Service.Migration.Metamodel.MigratorService where

import Control.Monad.Except (throwError)
import Data.Aeson

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import qualified Wizard.Service.Migration.Metamodel.Migrator.BranchMigrator as BranchMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator as KMMMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.PackageBundleMigrator as PBMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.PackageMigrator as PackageMigrator

migratePackageBundle :: Value -> AppContextM Value
migratePackageBundle value =
  runInTransaction $
    let eResult = PBMigrator.migrate value
     in case eResult of
          Right result -> return result
          Left error -> throwError error

migrateCompleteDatabase :: AppContextM ()
migrateCompleteDatabase =
  runInTransaction $ do
    PackageMigrator.migrateAllInDB
    BranchMigrator.migrateAllInDB
    KMMMigrator.migrateAllInDB
