module Wizard.Database.Migration.Development.TemporaryFile.TemporaryFileMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Database.DAO.TemporaryFile.TemporaryFileDAO

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(TemporaryFile/TemporaryFile) started"
  deleteTemporaryFiles
  logInfo _CMP_MIGRATION "(TemporaryFile/TemporaryFile) ended"
