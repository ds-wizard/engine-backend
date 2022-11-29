module Registry.Service.ActionKey.ActionKeyService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Uuid

getActionKeyByHash :: Maybe String -> AppContextM ActionKey
getActionKeyByHash mHash =
  case mHash of
    Just hash -> do
      mActionKey <- findActionKeyByHash' hash
      case mActionKey of
        Just actionKey -> return actionKey
        Nothing -> throwError $ UserError _ERROR_VALIDATION__HASH_ABSENCE
    Nothing -> throwError $ UserError _ERROR_VALIDATION__HASH_ABSENCE

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
