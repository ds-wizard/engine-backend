module Service.KnowledgeModelContainer.KnowledgeModelContainerService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Common.Types
import Common.Uuid
import Context
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Service.KnowledgeModelContainer.KnowledgeModelContainerMapper

getKnowledgeModelContainers :: Context -> IO [KnowledgeModelContainerDTO]
getKnowledgeModelContainers context = do
  kms <- findKnowledgeModelContainers context
  return . fmap toDTO $ kms

createKnowledgeModelContainer :: Context
                              -> KnowledgeModelContainerDTO
                              -> IO KnowledgeModelContainerDTO
createKnowledgeModelContainer context kmcDto = do
  let kmc = fromDTO kmcDto
  insertKnowledgeModelContainer context kmc
  return $ toDTO kmc

getKnowledgeModelContainerById :: Context
                               -> String
                               -> IO (Maybe KnowledgeModelContainerDTO)
getKnowledgeModelContainerById context kmcUuid = do
  maybeKM <- findKnowledgeModelContainerById context kmcUuid
  case maybeKM of
    Just km -> return . Just $ toDTO km
    Nothing -> return Nothing

modifyKnowledgeModelContainer
  :: Context
  -> String
  -> KnowledgeModelContainerDTO
  -> IO (Maybe KnowledgeModelContainerDTO)
modifyKnowledgeModelContainer context kmcUuid kmcDto = do
  maybeKmc <- findKnowledgeModelContainerById context kmcUuid
  case maybeKmc of
    Just kmc -> do
      let kmc = fromDTO kmcDto
      updateKnowledgeModelContainerById context kmc
      return . Just $ kmcDto
    Nothing -> return Nothing

deleteKnowledgeModelContainer :: Context -> String -> IO Bool
deleteKnowledgeModelContainer context kmcUuid = do
  maybeKmc <- findKnowledgeModelContainerById context kmcUuid
  case maybeKmc of
    Just kmc -> do
      deleteKnowledgeModelContainerById context kmcUuid
      return True
    Nothing -> return False
