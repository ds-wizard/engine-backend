module Database.Migration.Development.Migration.Questionnaire.MigratorMigration where

import Constant.Component
import Database.DAO.Migration.Questionnaire.MigratorDAO
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Migration/Questionnaire) started"
  deleteMigratorStates
  logInfo $ msg _CMP_MIGRATION "(Migration/Questionnaire) ended"
