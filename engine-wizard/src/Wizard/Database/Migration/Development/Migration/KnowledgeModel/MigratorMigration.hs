module Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration where

import Wizard.Constant.Component
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) started"
  deleteMigratorStates
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) ended"
