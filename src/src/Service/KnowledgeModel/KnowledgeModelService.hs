module Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resources.KnowledgeModel.KnowledgeModelDTO
import Common.Types
import Common.Uuid
import Context
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Model.KnowledgeModel
import Service.KnowledgeModel.KnowledgeModelMapper

getKnowledgeModels :: Context -> IO [KnowledgeModelDTO]
getKnowledgeModels context = do
    kms <- findKnowledgeModels context
    return . fmap toKnowledgeModelDTO $ kms

-- createUser :: Context -> UserCreateDTO -> IO UserDTO
-- createUser context userCreateDto = do
--     uuid <- generateUuid
--     createUserWithGivenUuid context uuid userCreateDto

-- createUserWithGivenUuid :: Context -> U.UUID -> UserCreateDTO -> IO UserDTO
-- createUserWithGivenUuid context userUuid userCreateDto = do
--     let roles = getPermissionForRole (userCreateDto ^. ucdtoRole)
--     passwordHash <- makePassword (BS.pack (userCreateDto ^. ucdtoPassword)) 17
--     let user =
--         fromUserCreateDTO userCreateDto userUuid (BS.unpack passwordHash) roles
--     insertUser context user
--     return $ toDTO user

getKnowledgeModelById :: Context -> String -> IO (Maybe KnowledgeModelDTO)
getKnowledgeModelById context kmUuid = do
    maybeKM <- findKnowledgeModelById context kmUuid
    case maybeKM of
        Just km -> return . Just $ toKnowledgeModelDTO km
        Nothing -> return Nothing

-- modifyUser :: Context -> String -> UserDTO -> IO (Maybe UserDTO)
-- modifyUser context userUuid userDto = do
--     maybeUser <- findUserById context userUuid
--     case maybeUser of
--     Just user -> do
--         let user = fromUserDTO userDto (user ^. uUuid) (user ^. uPasswordHash)
--         updateUserById context user
--         return . Just $ userDto
--     Nothing -> return Nothing

-- deleteUser :: Context -> String -> IO Bool
-- deleteUser context userUuid = do
--     maybeUser <- findUserById context userUuid
--     case maybeUser of
--     Just user -> do
--         deleteUserById context userUuid
--         return True
--     Nothing -> return False
        