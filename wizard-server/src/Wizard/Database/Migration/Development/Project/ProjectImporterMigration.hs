module Wizard.Database.Migration.Development.Project.ProjectImporterMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Project.ProjectImporterDAO
import Wizard.Database.Migration.Development.Project.Data.ProjectImporters
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(ProjectImporter/ProjectImporter) started"
  deleteProjectImporters
  insertProjectImporter projectImporterBio1
  insertProjectImporter projectImporterBio2
  insertProjectImporter projectImporterBio3
  insertProjectImporter projectImporterExt1
  insertProjectImporter projectImporterOnto1
  logInfo _CMP_MIGRATION "(ProjectImporter/ProjectImporter) ended"
