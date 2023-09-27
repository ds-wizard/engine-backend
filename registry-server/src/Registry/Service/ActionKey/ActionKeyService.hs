module Registry.Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Registry.Database.Mapping.ActionKey.ActionKeyType ()
import Registry.Model.ActionKey.ActionKeyType
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Util.Uuid

createActionKey :: String -> ActionKeyType -> AppContextM (ActionKey String ActionKeyType)
createActionKey orgId actionType = do
  uuid <- liftIO generateUuid
  hash <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let actionKey =
        ActionKey
          { uuid = uuid
          , identity = orgId
          , aType = actionType
          , hash = U.toString hash
          , tenantUuid = U.nil
          , createdAt = now
          }
  insertActionKey actionKey
  return actionKey
