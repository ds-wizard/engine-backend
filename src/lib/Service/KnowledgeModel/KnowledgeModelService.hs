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
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.Package.PackageDAO
import Model.Branch.Branch
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Package.Package
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
        Nothing -> return . Left $ NotExistsError "KM does not exist"
    Left error -> return . Left $ error

recompileKnowledgeModel :: Context -> String -> IO (Either AppError KnowledgeModel)
recompileKnowledgeModel context branchUuid =
  getBranch branchUuid $ \branch ->
    getEvents branch $ \events -> do
      let eitherNewKM = runApplicator Nothing events
      case eitherNewKM of
        Right newKM -> do
          updateKnowledgeModelByBranchId context branchUuid (Just newKM)
          return . Right $ newKM
        Left error -> return . Left $ error
  where
    getBranch branchUuid callback = do
      eitherBranch <- findBranchWithEventsById context branchUuid
      case eitherBranch of
        Right branch -> callback branch
        Left error -> return . Left $ error
    getEvents branch callback =
      case branch ^. bweParentPackageId of
        Just ppId -> do
          eitherEventsFromPackage <- getAllPreviousEventsSincePackageId context ppId
          case eitherEventsFromPackage of
            Right eventsFromPackage -> do
              let eventsFromKM = branch ^. bweEvents
              let events = eventsFromPackage ++ eventsFromKM
              callback events
            Left error -> return . Left $ error
        Nothing -> do
          let events = branch ^. bweEvents
          callback events
