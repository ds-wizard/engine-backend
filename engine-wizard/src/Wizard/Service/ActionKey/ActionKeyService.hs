module Wizard.Service.ActionKey.ActionKeyService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.Common
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Context.AppContext

getActionKeyByHash :: Maybe String -> AppContextM ActionKey
getActionKeyByHash mHash =
  runInTransaction $
  case mHash of
    Just hash -> do
      mActionKey <- findActionKeyByHash' hash
      case mActionKey of
        Just actionKey -> return actionKey
        Nothing -> throwError $ UserError _ERROR_VALIDATION__HASH_ABSENCE
    Nothing -> throwError $ UserError _ERROR_VALIDATION__HASH_ABSENCE

createActionKey :: U.UUID -> ActionKeyType -> U.UUID -> AppContextM ActionKey
createActionKey userId actionType appUuid =
  runInTransaction $ do
    uuid <- liftIO generateUuid
    hash <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let actionKey =
          ActionKey
            { _actionKeyUuid = uuid
            , _actionKeyUserId = userId
            , _actionKeyAType = actionType
            , _actionKeyHash = U.toString hash
            , _actionKeyAppUuid = appUuid
            , _actionKeyCreatedAt = now
            }
    insertActionKey actionKey
    return actionKey
