module Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorMigration where

import Wizard.Constant.Component
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Migration/Questionnaire) started"
  deleteMigratorStates
  logInfo _CMP_MIGRATION "(Migration/Questionnaire) ended"
