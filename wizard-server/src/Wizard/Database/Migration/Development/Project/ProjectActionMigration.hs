module Wizard.Database.Migration.Development.Project.ProjectActionMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Project.ProjectActionDAO
import Wizard.Database.Migration.Development.Project.Data.ProjectActions
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(ProjectAction/ProjectAction) started"
  deleteProjectActions
  insertProjectAction projectActionFtp1
  insertProjectAction projectActionFtp2
  insertProjectAction projectActionFtp3
  insertProjectAction projectActionMail1
  insertProjectAction projectActionScp1
  logInfo _CMP_MIGRATION "(ProjectAction/ProjectAction) ended"
