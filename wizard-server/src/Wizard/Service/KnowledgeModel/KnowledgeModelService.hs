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
  checkViewPermissionToKnowledgeModelPackage reqDto.knowledgeModelPackageUuid _PRJ_PERM
  compileKnowledgeModel reqDto.events reqDto.knowledgeModelPackageUuid reqDto.tagUuids

compileKnowledgeModel :: [KnowledgeModelEvent] -> Maybe U.UUID -> [U.UUID] -> AppContextM KnowledgeModel
compileKnowledgeModel events mPkgUuid tagUuids = compileKnowledgeModelWithCaching' events mPkgUuid tagUuids True

compileKnowledgeModelWithCaching' :: [KnowledgeModelEvent] -> Maybe U.UUID -> [U.UUID] -> Bool -> AppContextM KnowledgeModel
compileKnowledgeModelWithCaching' events mPkgUuid tagUuids useCache = do
  case (events, mPkgUuid) of
    ([], Just pkgUuid) -> do
      tenantUuid <- asks currentTenantUuid
      mKmCache <-
        if useCache
          then findKnowledgeModelCacheByUuid' pkgUuid tagUuids tenantUuid
          else return Nothing
      case mKmCache of
        Just kmCache -> return kmCache.knowledgeModel
        Nothing -> do
          allEvents <- getEvents mPkgUuid
          km <- liftEither $ compile Nothing allEvents
          let filteredKm = filterKnowledgeModel tagUuids km
          if useCache
            then do
              createdAt <- liftIO getCurrentTime
              let kmCache = KnowledgeModelCache {knowledgeModelPackageUuid = pkgUuid, tagUuids = tagUuids, knowledgeModel = filteredKm, tenantUuid = tenantUuid, createdAt = createdAt}
              insertKnowledgeModelCache kmCache
              return filteredKm
            else return filteredKm
    _ -> do
      allEvents <- getEvents mPkgUuid
      km <- liftEither $ compile Nothing allEvents
      return $ filterKnowledgeModel tagUuids km
  where
    getEvents :: Maybe U.UUID -> AppContextM [KnowledgeModelEvent]
    getEvents Nothing = return events
    getEvents (Just pkgUuid) = do
      pkgs <- findSeriesOfPackagesRecursiveByUuid pkgUuid :: AppContextM [KnowledgeModelBundlePackage]
      return $ concatMap (.events) pkgs ++ events
