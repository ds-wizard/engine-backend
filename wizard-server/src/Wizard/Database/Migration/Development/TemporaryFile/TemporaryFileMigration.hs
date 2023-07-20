module Wizard.Database.Migration.Development.TemporaryFile.TemporaryFileMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.TemporaryFile.TemporaryFileDAO

runMigration = do
  logInfo _CMP_MIGRATION "(TemporaryFile/TemporaryFile) started"
  deleteTemporaryFiles
  logInfo _CMP_MIGRATION "(TemporaryFile/TemporaryFile) ended"
