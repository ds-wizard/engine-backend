module Wizard.Service.KnowledgeModel.KnowledgeModelService where

import Control.Monad (when)
import Control.Monad.Except (liftEither)
import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Wizard.Cache.KnowledgeModelCache
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.KnowledgeModel.KnowledgeModelFilter
import Wizard.Service.Package.PackageUtil
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

createKnowledgeModelPreview :: KnowledgeModelChangeDTO -> AppContextM KnowledgeModel
createKnowledgeModelPreview reqDto = do
  mResolvedPackageId <- traverse resolvePackageId reqDto.packageId
  checkIfPackageIsPublic mResolvedPackageId _QTN_PERM
  compileKnowledgeModel reqDto.events mResolvedPackageId reqDto.tagUuids

compileKnowledgeModel :: [Event] -> Maybe String -> [U.UUID] -> AppContextM KnowledgeModel
compileKnowledgeModel events mPackageId tagUuids = compileKnowledgeModelWithCaching' events mPackageId tagUuids True

compileKnowledgeModelWithCaching' :: [Event] -> Maybe String -> [U.UUID] -> Bool -> AppContextM KnowledgeModel
compileKnowledgeModelWithCaching' events mPackageId tagUuids useCache = do
  mResolvedPackageId <- traverse resolvePackageId mPackageId
  case (events, mResolvedPackageId) of
    ([], Just resolvedPackageId) -> do
      tenantUuid <- asks currentTenantUuid
      mKm <-
        if useCache
          then getFromCache (resolvedPackageId, tagUuids, tenantUuid)
          else return Nothing
      case mKm of
        Just km -> return km
        Nothing -> do
          allEvents <- getEvents mResolvedPackageId
          km <- liftEither $ compile Nothing allEvents
          let filteredKm = filterKnowledgeModel tagUuids km
          when
            useCache
            (addToCache (resolvedPackageId, tagUuids, tenantUuid) filteredKm)
          return filteredKm
    _ -> do
      allEvents <- getEvents mPackageId
      km <- liftEither $ compile Nothing allEvents
      return $ filterKnowledgeModel tagUuids km
  where
    getEvents :: Maybe String -> AppContextM [Event]
    getEvents Nothing = return events
    getEvents (Just packageId) = do
      pkgs <- findSeriesOfPackagesRecursiveById packageId :: AppContextM [PackageWithEvents]
      return $ concatMap (.events) pkgs ++ events
