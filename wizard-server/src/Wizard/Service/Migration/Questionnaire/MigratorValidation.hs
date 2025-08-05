module Wizard.Service.Migration.Questionnaire.MigratorValidation where

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Localization.Messages.Public

validateMigrationExistence oldQtnUuid = do
  states <- findMigratorStatesByOldQuestionnaireUuid oldQtnUuid
  unless (null states) (throwError . UserError $ _ERROR_VALIDATION__QTN_MIGRATION_UNIQUENESS)
