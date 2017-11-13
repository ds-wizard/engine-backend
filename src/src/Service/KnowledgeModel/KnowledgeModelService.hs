module Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import Data.UUID as U

import Api.Resources.KnowledgeModel.KnowledgeModelDTO
import Common.Error
import Common.Types
import Common.Uuid
import Context
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Service.KnowledgeModel.KnowledgeModelMapper

getKnowledgeModelByKmcId :: Context
                         -> String
                         -> IO (Either AppError KnowledgeModelDTO)
getKnowledgeModelByKmcId context kmcUuid = do
  eitherKmcWithKm <- findKnowledgeModelByKmcId context kmcUuid
  case eitherKmcWithKm of
    Right kmcWithKm -> do
      let mKm = kmcWithKm ^. kmcwkmKM
      case mKm of
        Just km -> return . Right $ toKnowledgeModelDTO km
        Nothing -> return . Left $ NotExistsError "KM does not exist"
    Left error -> return . Left $ error
