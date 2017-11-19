module Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import Data.UUID as U

import Api.Resources.KnowledgeModel.KnowledgeModelDTO
import Common.Context
import Common.Error
import Common.Types
import Common.Uuid
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Model.KnowledgeModel.KnowledgeModel
import Model.Branch.Branch
import Service.KnowledgeModel.KnowledgeModelMapper

getKnowledgeModelByBranchId :: Context
                         -> String
                         -> IO (Either AppError KnowledgeModelDTO)
getKnowledgeModelByBranchId context branchUuid = do
  eitherBranchWithKm <- findKnowledgeModelByBranchId context branchUuid
  case eitherBranchWithKm of
    Right branchWithKm -> do
      let mKm = branchWithKm ^. bwkmKM
      case mKm of
        Just km -> return . Right $ toKnowledgeModelDTO km
        Nothing -> return . Left $ NotExistsError "KM does not exist"
    Left error -> return . Left $ error
