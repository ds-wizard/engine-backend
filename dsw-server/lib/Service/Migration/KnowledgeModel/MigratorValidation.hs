module Service.Migration.KnowledgeModel.MigratorValidation where

import Data.Maybe

import Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Service.Package.PackageUtils
import Service.Package.PackageValidation
import Util.Helper (createHmeHelper)

validateMigrationUniqueness :: String -> AppContextM (Maybe AppError)
validateMigrationUniqueness bUuid = do
  eResult <- findMigratorStateByBranchUuid bUuid
  case eResult of
    Left (NotExistsError _) -> return Nothing
    Right _ -> return . Just . MigratorError $ _ERROR_KMMT_VALIDATION_MIGRATOR__MIGRATION_UNIQUENESS
    Left error -> return . Just $ error

validateIfTargetPackageVersionIsHigher :: String -> String -> AppContextM (Maybe AppError)
validateIfTargetPackageVersionIsHigher forkOfPackageId targetPackageId = do
  let targetPackageVersion = getVersionFromPkgId targetPackageId
  let forkOfPackageIdVersion = getVersionFromPkgId forkOfPackageId
  if isNothing $ validateIsVersionHigher targetPackageVersion forkOfPackageIdVersion
    then return Nothing
    else return . Just . MigratorError $ _ERROR_KMMT_MIGRATOR__TARGET_PKG_IS_NOT_HIGHER

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateMigrationUniqueness bUuid = createHmeHelper (validateMigrationUniqueness bUuid)

-- -----------------------------------------------------
heValidateIfTargetPackageVersionIsHigher forkOfPackageId targetPackageId =
  createHmeHelper (validateIfTargetPackageVersionIsHigher forkOfPackageId targetPackageId)
