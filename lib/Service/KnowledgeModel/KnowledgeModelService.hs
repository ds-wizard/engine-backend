module Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))

import Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.KnowledgeModel.KnowledgeModelFilter
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Package.PackageService

getKnowledgeModelByBranchId :: String -> AppContextM (Either AppError KnowledgeModelDTO)
getKnowledgeModelByBranchId branchUuid =
  heFindBranchWithKMByBranchId branchUuid $ \branchWithKm -> do
    let mKm = branchWithKm ^. knowledgeModel
    case mKm of
      Just km -> return . Right $ toKnowledgeModelDTO km
      Nothing -> return . Left . NotExistsError $ _ERROR_VALIDATION__KM_ABSENCE

recompileKnowledgeModel :: String -> AppContextM (Either AppError KnowledgeModel)
recompileKnowledgeModel branchUuid =
  heGetEventsForBranchUuid branchUuid $ \eventsForBranchUuid ->
    recompileKnowledgeModelWithEvents branchUuid eventsForBranchUuid

createKnowledgeModelPreview :: KnowledgeModelChangeDTO -> AppContextM (Either AppError KnowledgeModelDTO)
createKnowledgeModelPreview reqDto =
  heGetEvents $ \events ->
    heCompileKnowledgeModelFromScratch events $ \km -> do
      let filteredKm = filterKnowledgeModel (reqDto ^. tagUuids) km
      return . Right . toKnowledgeModelDTO $ km
  where
    requestEvents = fromDTOs $ reqDto ^. events
    heGetEvents callback =
      case reqDto ^. packageId of
        Just packageId ->
          heGetAllPreviousEventsSincePackageId packageId $ \packageEvents -> do
            callback $ packageEvents ++ requestEvents
        Nothing -> callback requestEvents

-- --------------------------------
-- HELPERS
-- --------------------------------
heRecompileKnowledgeModel branchUuid callback = do
  eitherKM <- recompileKnowledgeModel branchUuid
  case eitherKM of
    Right km -> callback km
    Left error -> return . Left $ error

hmRecompileKnowledgeModel branchUuid callback = do
  eitherKM <- recompileKnowledgeModel branchUuid
  case eitherKM of
    Right km -> callback km
    Left error -> return . Just $ error
