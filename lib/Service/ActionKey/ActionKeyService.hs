module Service.ActionKey.ActionKeyService where

import Data.UUID

import Common.Context
import Common.Error
import Common.Uuid
import Database.DAO.ActionKey.ActionKeyDAO
import Model.ActionKey.ActionKey

getActionKeyByHash :: Context -> String -> IO (Either AppError ActionKey)
getActionKeyByHash = findActionKeyByHash

createActionKey :: Context -> UUID -> ActionKeyType -> IO (Either AppError ActionKey)
createActionKey context userId actionType = do
  uuid <- generateUuid
  hash <- generateUuid
  let actionKey =
        ActionKey
        {_actionKeyUuid = uuid, _actionKeyUserId = userId, _actionKeyAType = actionType, _actionKeyHash = toString hash}
  insertActionKey context actionKey
  return . Right $ actionKey

deleteActionKey :: Context -> String -> IO (Maybe AppError)
deleteActionKey context hash = do
  eitherActionKey <- getActionKeyByHash context hash
  case eitherActionKey of
    Right actionKey -> do
      deleteActionKeyByHash context hash
      return Nothing
    Left error -> return . Just $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetActionKeyByHash context akHash callback = do
  eitherActionKey <- getActionKeyByHash context akHash
  case eitherActionKey of
    Right actionKey -> callback actionKey
    Left error -> return . Left $ error

hmGetActionKeyByHash context akHash callback = do
  eitherActionKey <- getActionKeyByHash context akHash
  case eitherActionKey of
    Right actionKey -> callback actionKey
    Left error -> return . Just $ error

-- -----------------------------------------------------
heCreateActionKey context userUuid actionType callback = do
  eitherActionKey <- createActionKey context userUuid actionType
  case eitherActionKey of
    Right actionKey -> callback actionKey
    Left error -> return . Left $ error

hmCreateActionKey context userUuid actionType callback = do
  eitherActionKey <- createActionKey context userUuid actionType
  case eitherActionKey of
    Right actionKey -> callback actionKey
    Left error -> return . Just $ error
