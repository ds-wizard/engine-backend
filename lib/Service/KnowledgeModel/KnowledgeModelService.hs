module Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Package.PackageService

getKnowledgeModelByBranchId :: String -> AppContextM (Either AppError KnowledgeModelDTO)
getKnowledgeModelByBranchId branchUuid = do
  eitherBranchWithKm <- findBranchWithKMByBranchId branchUuid
  case eitherBranchWithKm of
    Right branchWithKm -> do
      let mKm = branchWithKm ^. knowledgeModel
      case mKm of
        Just km -> return . Right $ toKnowledgeModelDTO km
        Nothing -> return . Left . NotExistsError $ _ERROR_VALIDATION__KM_ABSENCE
    Left error -> return . Left $ error

recompileKnowledgeModel :: String -> AppContextM (Either AppError KnowledgeModel)
recompileKnowledgeModel branchUuid = do
  eitherEventsForUuid <- getEventsForBranchUuid branchUuid
  case eitherEventsForUuid of
    Right eventsForBranchUuid -> recompileKnowledgeModelWithEvents branchUuid eventsForBranchUuid
    Left error -> return . Left $ error
