module Wizard.Service.KnowledgeModel.KnowledgeModelService where

import Control.Monad.Except (liftEither)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO
import Wizard.Database.Mapping.KnowledgeModel.Bundle.KnowledgeModelBundlePackage ()
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.KnowledgeModelCache
import Wizard.Service.KnowledgeModel.Compiler.Compiler
import Wizard.Service.KnowledgeModel.KnowledgeModelFilter
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil

createKnowledgeModelPreview :: KnowledgeModelChangeDTO -> AppContextM KnowledgeModel
createKnowledgeModelPreview reqDto = do
  mResolvedPackageId <- traverse resolvePackageId reqDto.knowledgeModelPackageId
  checkIfPackageIsPublic mResolvedPackageId _PRJ_PERM
  compileKnowledgeModel reqDto.events mResolvedPackageId reqDto.tagUuids

compileKnowledgeModel :: [KnowledgeModelEvent] -> Maybe String -> [U.UUID] -> AppContextM KnowledgeModel
compileKnowledgeModel events mPackageId tagUuids = compileKnowledgeModelWithCaching' events mPackageId tagUuids True

compileKnowledgeModelWithCaching' :: [KnowledgeModelEvent] -> Maybe String -> [U.UUID] -> Bool -> AppContextM KnowledgeModel
compileKnowledgeModelWithCaching' events mPackageId tagUuids useCache = do
  mResolvedPackageId <- traverse resolvePackageId mPackageId
  case (events, mResolvedPackageId) of
    ([], Just resolvedPackageId) -> do
      tenantUuid <- asks currentTenantUuid
      mKmCache <-
        if useCache
          then findKnowledgeModelCacheById' resolvedPackageId tagUuids tenantUuid
          else return Nothing
      case mKmCache of
        Just kmCache -> return kmCache.knowledgeModel
        Nothing -> do
          allEvents <- getEvents mResolvedPackageId
          km <- liftEither $ compile Nothing allEvents
          let filteredKm = filterKnowledgeModel tagUuids km
          if useCache
            then do
              createdAt <- liftIO getCurrentTime
              let kmCache = KnowledgeModelCache {knowledgeModelPackageId = resolvedPackageId, tagUuids = tagUuids, knowledgeModel = filteredKm, tenantUuid = tenantUuid, createdAt = createdAt}
              insertKnowledgeModelCache kmCache
              return filteredKm
            else return filteredKm
    _ -> do
      allEvents <- getEvents mPackageId
      km <- liftEither $ compile Nothing allEvents
      return $ filterKnowledgeModel tagUuids km
  where
    getEvents :: Maybe String -> AppContextM [KnowledgeModelEvent]
    getEvents Nothing = return events
    getEvents (Just packageId) = do
      pkgs <- findSeriesOfPackagesRecursiveById packageId :: AppContextM [KnowledgeModelBundlePackage]
      return $ concatMap (.events) pkgs ++ events
