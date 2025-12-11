module Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Service.Coordinate.CoordinateValidation
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Localization.Messages.Public
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation

validateMigrationExistence editorUuid = do
  mMs <- findKnowledgeModelMigrationByEditorUuid' editorUuid
  when (isJust mMs) (throwError . UserError $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__KM_MIGRATION_EXISTS)

validateNewPackageVersion pkgVersion kmEditor org = do
  validateVersionFormat False pkgVersion
  mPkg <- getTheNewestPackageByOrganizationIdAndKmId org.organizationId kmEditor.kmId
  case mPkg of
    Just pkg -> validateIsVersionHigher pkgVersion pkg.version
    Nothing -> return ()
