module Shared.ActionKey.Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Database.PostgreSQL.Simple.ToField
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Date
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid

createActionKey
  :: (AppContextC s sc m, ToField aType, ToField identity)
  => identity
  -> aType
  -> U.UUID
  -> m (ActionKey identity aType)
createActionKey identity actionType appUuid = do
  hash <- liftIO generateUuid
  createActionKeyWithHash identity actionType appUuid (U.toString hash)

createActionKeyWithHash
  :: (AppContextC s sc m, ToField aType, ToField identity)
  => identity
  -> aType
  -> U.UUID
  -> String
  -> m (ActionKey identity aType)
createActionKeyWithHash identity actionType appUuid hash =
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let actionKey =
          ActionKey
            { uuid = uuid
            , identity = identity
            , aType = actionType
            , hash = hash
            , appUuid = appUuid
            , createdAt = now
            }
    insertActionKey actionKey
    return actionKey

cleanActionKeys :: AppContextC s sc m => m ()
cleanActionKeys = do
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ nominalDayInSeconds * (-1)
  let dayBefore = addUTCTime timeDelta now
  deleteActionKeyOlderThen dayBefore
  return ()
