module Wizard.Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.ActionKey.ActionKeyType ()
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Context.AppContext (AppContextM)

createActionKey :: U.UUID -> ActionKeyType -> U.UUID -> AppContextM (ActionKey U.UUID ActionKeyType)
createActionKey userUuid actionType tenantUuid = do
  hash <- liftIO generateUuid
  createActionKeyWithHash userUuid actionType tenantUuid (U.toString hash)

createActionKeyWithHash :: U.UUID -> ActionKeyType -> U.UUID -> String -> AppContextM (ActionKey U.UUID ActionKeyType)
createActionKeyWithHash userUuid actionType tenantUuid hash =
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let actionKey =
          ActionKey
            { uuid = uuid
            , identity = userUuid
            , aType = actionType
            , hash = hash
            , tenantUuid = tenantUuid
            , createdAt = now
            }
    insertActionKey actionKey
    return actionKey

cleanActionKeys :: AppContextM ()
cleanActionKeys = do
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ nominalDayInSeconds * (-1) * 14
  let dayBefore = addUTCTime timeDelta now
  deleteActionKeyOlderThen dayBefore
  return ()
