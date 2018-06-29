module Service.ActionKey.ActionKeyService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Common.Error
import Common.Uuid
import Database.DAO.ActionKey.ActionKeyDAO
import Model.ActionKey.ActionKey
import Model.Context.AppContext

getActionKeyByHash :: String -> AppContextM (Either AppError ActionKey)
getActionKeyByHash = findActionKeyByHash

createActionKey :: U.UUID -> ActionKeyType -> AppContextM (Either AppError ActionKey)
createActionKey userId actionType = do
  uuid <- liftIO generateUuid
  hash <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let actionKey =
        ActionKey
        { _actionKeyUuid = uuid
        , _actionKeyUserId = userId
        , _actionKeyAType = actionType
        , _actionKeyHash = U.toString hash
        , _actionKeyCreatedAt = now
        }
  insertActionKey actionKey
  return . Right $ actionKey

deleteActionKey :: String -> AppContextM (Maybe AppError)
deleteActionKey hash = do
  eitherActionKey <- getActionKeyByHash hash
  case eitherActionKey of
    Right actionKey -> do
      deleteActionKeyByHash hash
      return Nothing
    Left error -> return . Just $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetActionKeyByHash akHash callback = do
  eitherActionKey <- getActionKeyByHash akHash
  case eitherActionKey of
    Right actionKey -> callback actionKey
    Left error -> return . Left $ error

hmGetActionKeyByHash akHash callback = do
  eitherActionKey <- getActionKeyByHash akHash
  case eitherActionKey of
    Right actionKey -> callback actionKey
    Left error -> return . Just $ error

-- -----------------------------------------------------
heCreateActionKey userUuid actionType callback = do
  eitherActionKey <- createActionKey userUuid actionType
  case eitherActionKey of
    Right actionKey -> callback actionKey
    Left error -> return . Left $ error

hmCreateActionKey userUuid actionType callback = do
  eitherActionKey <- createActionKey userUuid actionType
  case eitherActionKey of
    Right actionKey -> callback actionKey
    Left error -> return . Just $ error
