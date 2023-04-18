module Wizard.Database.Migration.Development.TemporaryFile.TemporaryFileMigration where

import Shared.Common.Constant.Component
import Wizard.Database.DAO.TemporaryFile.TemporaryFileDAO
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(TemporaryFile/TemporaryFile) started"
  deleteTemporaryFiles
  logInfo _CMP_MIGRATION "(TemporaryFile/TemporaryFile) ended"
