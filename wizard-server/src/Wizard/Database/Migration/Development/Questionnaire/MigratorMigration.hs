module Wizard.Database.Migration.Development.Questionnaire.MigratorMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Questionnaire.MigratorDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.MigratorStates

runMigration = do
  logInfo _CMP_MIGRATION "(Migration/Questionnaire) started"
  deleteMigratorStates
  insertMigratorState differentQtnMigrationState
  logInfo _CMP_MIGRATION "(Migration/Questionnaire) ended"
