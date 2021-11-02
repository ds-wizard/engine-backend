module Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Migration/Questionnaire) started"
  deleteMigratorStates
  insertMigratorState differentQtnMigrationState
  logInfo _CMP_MIGRATION "(Migration/Questionnaire) ended"
