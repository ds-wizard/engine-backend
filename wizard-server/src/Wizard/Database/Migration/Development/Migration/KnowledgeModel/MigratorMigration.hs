module Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations

runMigration = do
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) started"
  deleteMigratorStates
  insertMigratorState differentMigratorState
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) ended"
