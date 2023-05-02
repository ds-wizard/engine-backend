module Wizard.Service.Package.Publish.PackagePublishValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)

import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Localization.Messages.Public
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageValidation
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.KnowledgeModel.Model.Package.Package

validateMigrationExistence branchUuid = do
  mMs <- findMigratorStateByBranchUuid' branchUuid
  when (isJust mMs) (throwError . UserError $ _ERROR_SERVICE_BRANCH__KM_MIGRATION_EXISTS)

validateNewPackageVersion pkgVersion branch org = do
  validateVersionFormat False pkgVersion
  mPkg <- getTheNewestPackageByOrganizationIdAndKmId org.organizationId branch.kmId
  case mPkg of
    Just pkg -> validateIsVersionHigher pkgVersion pkg.version
    Nothing -> return ()
