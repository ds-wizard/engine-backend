module Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration where

import Shared.Common.Constant.Component
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) started"
  deleteMigratorStates
  insertMigratorState differentMigratorState
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) ended"
