module Wizard.Service.Migration.KnowledgeModel.MigratorValidation where

import Control.Monad.Except (catchError, throwError)

import Shared.Model.Error.Error
import Shared.Util.Identifier
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageValidation

validateMigrationUniqueness :: String -> AppContextM ()
validateMigrationUniqueness bUuid = do
  mMs <- findMigratorStateByBranchUuid' bUuid
  case mMs of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS

validateIfTargetPackageVersionIsHigher :: String -> String -> AppContextM ()
validateIfTargetPackageVersionIsHigher forkOfPackageId targetPackageId = do
  let targetPackageVersion = getVersionFromPkgId targetPackageId
  let forkOfPackageIdVersion = getVersionFromPkgId forkOfPackageId
  catchError
    (validateIsVersionHigher targetPackageVersion forkOfPackageIdVersion)
    (\_ -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__TARGET_PKG_IS_NOT_HIGHER)
