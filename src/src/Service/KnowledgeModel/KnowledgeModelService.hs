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
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Service.KnowledgeModel.KnowledgeModelMapper

getKnowledgeModelByKmcId :: Context -> String -> IO (Maybe KnowledgeModelDTO)
getKnowledgeModelByKmcId context kmcUuid = do
  maybeKmcWithKm <- findKnowledgeModelByKmcId context kmcUuid
  case maybeKmcWithKm of
    Just kmcWithKm ->
      return . Just $ toKnowledgeModelDTO (kmcWithKm ^. kmcwkmKM)
    Nothing -> return Nothing
