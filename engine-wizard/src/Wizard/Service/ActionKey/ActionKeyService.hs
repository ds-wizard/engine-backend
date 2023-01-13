module Wizard.Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Util.Uuid
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.Common
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Context.AppContext (AppContextM)

createActionKey :: U.UUID -> ActionKeyType -> U.UUID -> AppContextM ActionKey
createActionKey userId actionType appUuid =
  runInTransaction $ do
    uuid <- liftIO generateUuid
    hash <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let actionKey =
          ActionKey
            { uuid = uuid
            , userId = userId
            , aType = actionType
            , hash = U.toString hash
            , appUuid = appUuid
            , createdAt = now
            }
    insertActionKey actionKey
    return actionKey
