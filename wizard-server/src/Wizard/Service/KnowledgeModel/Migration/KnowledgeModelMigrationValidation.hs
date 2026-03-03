module Wizard.Service.KnowledgeModel.Migration.KnowledgeModelMigrationValidation where

import Control.Monad.Except (catchError, throwError)
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Model.Coordinate.Coordinate
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation

validateMigrationUniqueness :: U.UUID -> AppContextM ()
validateMigrationUniqueness bUuid = do
  mMs <- findKnowledgeModelMigrationByEditorUuid' bUuid
  case mMs of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS

validateIfTargetPackageVersionIsHigher :: Coordinate -> Coordinate -> AppContextM ()
validateIfTargetPackageVersionIsHigher forkOfPackageId targetPackage =
  catchError
    (validateIsVersionHigher targetPackage.version forkOfPackageId.version)
    (\_ -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__TARGET_PKG_IS_NOT_HIGHER)
