module Registry.Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Shared.Util.Uuid

createActionKey :: String -> ActionKeyType -> AppContextM ActionKey
createActionKey orgId actionType = do
  uuid <- liftIO generateUuid
  hash <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let actionKey =
        ActionKey
          { uuid = uuid
          , organizationId = orgId
          , aType = actionType
          , hash = U.toString hash
          , createdAt = now
          }
  insertActionKey actionKey
  return actionKey
