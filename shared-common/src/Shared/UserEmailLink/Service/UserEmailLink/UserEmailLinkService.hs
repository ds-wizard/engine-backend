module Shared.UserEmailLink.Service.UserEmailLink.UserEmailLinkService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Database.PostgreSQL.Simple.ToField
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Date
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink

createUserEmailLink
  :: (AppContextC s sc m, ToField aType, ToField identity)
  => identity
  -> aType
  -> U.UUID
  -> m (UserEmailLink identity aType)
createUserEmailLink identity actionType tenantUuid = do
  hash <- liftIO generateUuid
  createUserEmailLinkWithHash identity actionType tenantUuid (U.toString hash)

createUserEmailLinkWithHash
  :: (AppContextC s sc m, ToField aType, ToField identity)
  => identity
  -> aType
  -> U.UUID
  -> String
  -> m (UserEmailLink identity aType)
createUserEmailLinkWithHash identity actionType tenantUuid hash =
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let userEmailLink =
          UserEmailLink
            { uuid = uuid
            , identity = identity
            , aType = actionType
            , hash = hash
            , tenantUuid = tenantUuid
            , createdAt = now
            }
    insertUserEmailLink userEmailLink
    return userEmailLink

cleanUserEmailLinks :: AppContextC s sc m => m ()
cleanUserEmailLinks = do
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ nominalDayInSeconds * (-1)
  let dayBefore = addUTCTime timeDelta now
  deleteUserEmailLinkOlderThen dayBefore
  return ()
