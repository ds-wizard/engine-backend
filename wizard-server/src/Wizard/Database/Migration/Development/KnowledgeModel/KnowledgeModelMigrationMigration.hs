module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations

runMigration = do
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) started"
  deleteMigratorStates
  insertMigratorState differentMigratorState
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) ended"
