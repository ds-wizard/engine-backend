module Wizard.Database.Migration.Development.Project.ProjectMigrationMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Project.ProjectMigrationDAO
import Wizard.Database.Migration.Development.Project.Data.ProjectMigrations

runMigration = do
  logInfo _CMP_MIGRATION "(Migration/Project) started"
  deleteProjectMigrations
  insertProjectMigration differentProjectMigration
  logInfo _CMP_MIGRATION "(Migration/Project) ended"
