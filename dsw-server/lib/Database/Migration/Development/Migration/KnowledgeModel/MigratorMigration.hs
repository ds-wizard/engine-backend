module Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration where

import Constant.Component
import Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Migration/KnowledgeModel) started"
  deleteMigratorStates
  logInfo $ msg _CMP_MIGRATION "(Migration/KnowledgeModel) ended"
