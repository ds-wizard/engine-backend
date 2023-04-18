module Wizard.Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.Common
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Context.AppContext (AppContextM)
import Wizard.Util.Date

createActionKey :: U.UUID -> ActionKeyType -> U.UUID -> AppContextM ActionKey
createActionKey userId actionType appUuid = do
  hash <- liftIO generateUuid
  createActionKeyWithHash userId actionType appUuid (U.toString hash)

createActionKeyWithHash :: U.UUID -> ActionKeyType -> U.UUID -> String -> AppContextM ActionKey
createActionKeyWithHash userId actionType appUuid hash =
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let actionKey =
          ActionKey
            { uuid = uuid
            , userId = userId
            , aType = actionType
            , hash = hash
            , appUuid = appUuid
            , createdAt = now
            }
    insertActionKey actionKey
    return actionKey

cleanActionKeys :: AppContextM ()
cleanActionKeys = do
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ nominalDayInSeconds * (-1)
  let dayBefore = addUTCTime timeDelta now
  deleteActionKeyOlderThen dayBefore
  return ()
