module Wizard.Service.Project.Migration.ProjectMigrationValidation where

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Project.ProjectMigrationDAO
import Wizard.Localization.Messages.Public

validateMigrationExistence oldProjectUuid = do
  states <- findProjectMigrationsByOldProjectUuid oldProjectUuid
  unless (null states) (throwError . UserError $ _ERROR_VALIDATION__PROJECT_MIGRATION_UNIQUENESS)
