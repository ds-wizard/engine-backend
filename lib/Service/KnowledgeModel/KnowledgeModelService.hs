module Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import Data.UUID as U

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Common.Context
import Common.Error
import Common.Localization
import Common.Types
import Common.Uuid
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.Package.PackageDAO
import Model.Branch.Branch
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Package.Package
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Migrator.Applicator
import Service.Package.PackageService

getKnowledgeModelByBranchId :: Context -> String -> IO (Either AppError KnowledgeModelDTO)
getKnowledgeModelByBranchId context branchUuid = do
  eitherBranchWithKm <- findBranchWithKMByBranchId context branchUuid
  case eitherBranchWithKm of
    Right branchWithKm -> do
      let mKm = branchWithKm ^. bwkmKM
      case mKm of
        Just km -> return . Right $ toKnowledgeModelDTO km
        Nothing -> return . Left . NotExistsError $ _ERROR_VALIDATION__KM_ABSENCE
    Left error -> return . Left $ error

recompileKnowledgeModel :: Context -> String -> IO (Either AppError KnowledgeModel)
recompileKnowledgeModel context branchUuid = do
  eitherEventsForUuid <- getEventsForBranchUuid context branchUuid
  case eitherEventsForUuid of
    Right eventsForBranchUuid -> recompileKnowledgeModelWithEvents context branchUuid eventsForBranchUuid
    Left error -> return . Left $ error
