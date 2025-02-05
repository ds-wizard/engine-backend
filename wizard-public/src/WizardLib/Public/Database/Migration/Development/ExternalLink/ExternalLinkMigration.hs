module WizardLib.Public.Database.Migration.Development.ExternalLink.ExternalLinkMigration where

import Shared.Common.Constant.Component
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import WizardLib.Public.Database.DAO.ExternalLink.ExternalLinkUsageDAO

runMigration :: AppContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(ExternalLink/ExternalLink) started"
  deleteExternalLinkUsages
  logInfo _CMP_MIGRATION "(ExternalLink/ExternalLink) ended"
