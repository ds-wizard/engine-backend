module Service.KnowledgeModelContainer.KnowledgeModelContainerService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import Data.UUID as U
import Text.Regex

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Common.Error
import Common.Types
import Common.Uuid
import Context
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Service.KnowledgeModelContainer.KnowledgeModelContainerMapper

getKnowledgeModelContainers :: Context
                            -> IO (Either AppError [KnowledgeModelContainerDTO])
getKnowledgeModelContainers context = do
  eitherKmcs <- findKnowledgeModelContainers context
  case eitherKmcs of
    Right kmcs -> return . Right . fmap toDTO $ kmcs
    Left error -> return . Left $ error

createKnowledgeModelContainer
  :: Context
  -> KnowledgeModelContainerDTO
  -> IO (Either AppError KnowledgeModelContainerDTO)
createKnowledgeModelContainer context kmcDto = do
  let artifactId = kmcDto ^. kmcdtoArtifactId
  case isValidArtifactId artifactId of
    Nothing -> do
      eitherKmcFromDb <-
        findKnowledgeModelContainerByArtifactId context artifactId
      case eitherKmcFromDb of
        Right _ ->
          return . Left $
          createErrorWithFieldError
            ("artifactId", "ArtifactId is already taken")
        Left (NotExistsError _) -> do
          let kmc = fromDTO kmcDto
          insertKnowledgeModelContainer context kmc
          return . Right . toDTO $ kmc
        Left error -> return . Left $ error
    Just error -> return . Left $ error

getKnowledgeModelContainerById :: Context
                               -> String
                               -> IO (Either AppError KnowledgeModelContainerDTO)
getKnowledgeModelContainerById context kmcUuid = do
  eitherKmc <- findKnowledgeModelContainerById context kmcUuid
  case eitherKmc of
    Right kmc -> return . Right . toDTO $ kmc
    Left error -> return . Left $ error

modifyKnowledgeModelContainer
  :: Context
  -> String
  -> KnowledgeModelContainerDTO
  -> IO (Either AppError KnowledgeModelContainerDTO)
modifyKnowledgeModelContainer context kmcUuid kmcDto = do
  let artifactId = kmcDto ^. kmcdtoArtifactId
  case isValidArtifactId artifactId of
    Nothing -> do
      eitherKmc <- findKnowledgeModelContainerById context kmcUuid
      case eitherKmc of
        Right kmc -> do
          eitherKmcFromDb <-
            findKnowledgeModelContainerByArtifactId context artifactId
          if isAlreadyUsedAndIsNotMine eitherKmcFromDb
            then return . Left . createErrorWithFieldError $
                 ("artifactId", "ArtifactId is already taken")
            else do
              let kmc = fromDTO kmcDto
              updateKnowledgeModelContainerById context kmc
              return . Right $ kmcDto
        Left error -> return . Left $ error
    Just error -> return . Left $ error
  where
    isAlreadyUsedAndIsNotMine (Right kmc) =
      U.toString (kmc ^. kmcKmcUuid) /= kmcUuid
    isAlreadyUsedAndIsNotMine (Left _) = False

deleteKnowledgeModelContainer :: Context -> String -> IO (Maybe AppError)
deleteKnowledgeModelContainer context kmcUuid = do
  eitherKmc <- findKnowledgeModelContainerById context kmcUuid
  case eitherKmc of
    Right kmc -> do
      deleteKnowledgeModelContainerById context kmcUuid
      return Nothing
    Left error -> return . Just $ error

isValidArtifactId :: String -> Maybe AppError
isValidArtifactId artifactId =
  if isJust $ matchRegex validationRegex artifactId
  then Nothing
  else Just $ createErrorWithFieldError ("artifactId", "ArtifactId is not in valid format")
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$"
