module Registry.Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Shared.Model.Error.Error
import Shared.Util.Uuid

getActionKeyByHash :: String -> AppContextM ActionKey
getActionKeyByHash = findActionKeyByHash

createActionKey :: String -> ActionKeyType -> AppContextM ActionKey
createActionKey orgId actionType = do
  uuid <- liftIO generateUuid
  hash <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let actionKey =
        ActionKey
          { _actionKeyUuid = uuid
          , _actionKeyOrganizationId = orgId
          , _actionKeyAType = actionType
          , _actionKeyHash = U.toString hash
          , _actionKeyCreatedAt = now
          }
  insertActionKey actionKey
  return actionKey

deleteActionKey :: String -> AppContextM (Maybe AppError)
deleteActionKey hash = do
  actionKey <- getActionKeyByHash hash
  deleteActionKeyByHash hash
  return Nothing
