module WizardLib.Public.Service.ExternalLink.ExternalLinkUsageService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import Prelude hiding (id)

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import WizardLib.Public.Database.DAO.ExternalLink.ExternalLinkUsageDAO
import WizardLib.Public.Model.ExternalLink.ExternalLinkUsage

createExternalLinkUsage :: AppContextC s sc m => String -> m ()
createExternalLinkUsage url =
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let externalLinkUsage = ExternalLinkUsage uuid url tenantUuid now
    void $ insertExternalLinkUsage externalLinkUsage
