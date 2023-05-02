module Wizard.Service.Migration.KnowledgeModel.MigratorValidation where

import Control.Monad.Except (catchError, throwError)
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageValidation
import WizardLib.Common.Util.Coordinate

validateMigrationUniqueness :: U.UUID -> AppContextM ()
validateMigrationUniqueness bUuid = do
  mMs <- findMigratorStateByBranchUuid' bUuid
  case mMs of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS

validateIfTargetPackageVersionIsHigher :: String -> String -> AppContextM ()
validateIfTargetPackageVersionIsHigher forkOfPackageId targetPackageId = do
  let targetPackageVersion = getVersionFromCoordinate targetPackageId
  let forkOfPackageIdVersion = getVersionFromCoordinate forkOfPackageId
  catchError
    (validateIsVersionHigher targetPackageVersion forkOfPackageIdVersion)
    (\_ -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__TARGET_PKG_IS_NOT_HIGHER)
