module Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Database.DAO.ActionKey.ActionKeyDAO
import Model.ActionKey.ActionKey
import Model.Context.AppContext
import Model.Error.Error
import Util.Helper
import Util.Uuid

getActionKeyByHash :: String -> AppContextM (Either AppError ActionKey)
getActionKeyByHash = findActionKeyByHash

createActionKey :: String -> ActionKeyType -> AppContextM (Either AppError ActionKey)
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
  return . Right $ actionKey

deleteActionKey :: String -> AppContextM (Maybe AppError)
deleteActionKey hash =
  hmGetActionKeyByHash hash $ \actionKey -> do
    deleteActionKeyByHash hash
    return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetActionKeyByHash hash callback = createHeeHelper (getActionKeyByHash hash) callback

hmGetActionKeyByHash hash callback = createHemHelper (getActionKeyByHash hash) callback

-- -----------------------------------------------------
heCreateActionKey orgId actionType callback = createHeeHelper (createActionKey orgId actionType) callback

hmCreateActionKey orgId actionType callback = createHemHelper (createActionKey orgId actionType) callback
