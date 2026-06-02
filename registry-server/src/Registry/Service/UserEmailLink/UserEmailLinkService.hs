module Registry.Service.UserEmailLink.UserEmailLinkService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Registry.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Model.UserEmailLink.UserEmailLinkType
import Shared.Common.Util.Uuid
import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink

createUserEmailLink :: String -> UserEmailLinkType -> AppContextM (UserEmailLink String UserEmailLinkType)
createUserEmailLink orgId actionType = do
  uuid <- liftIO generateUuid
  hash <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let userEmailLink =
        UserEmailLink
          { uuid = uuid
          , identity = orgId
          , aType = actionType
          , hash = U.toString hash
          , tenantUuid = U.nil
          , createdAt = now
          }
  insertUserEmailLink userEmailLink
  return userEmailLink
