module Wizard.Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))
import Control.Monad.Except (liftEither)
import qualified Data.UUID as U

import LensesConfig
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Context.AppContext
import Wizard.Service.Cache.KnowledgeModelCache
import Wizard.Service.Common.ACL
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.KnowledgeModel.KnowledgeModelFilter
import Wizard.Service.Package.PackageService

createKnowledgeModelPreview :: KnowledgeModelChangeDTO -> AppContextM KnowledgeModel
createKnowledgeModelPreview reqDto = do
  checkPermission _QTN_PERM
  compileKnowledgeModel (reqDto ^. events) (reqDto ^. packageId) (reqDto ^. tagUuids)

compileKnowledgeModel :: [Event] -> Maybe String -> [U.UUID] -> AppContextM KnowledgeModel
compileKnowledgeModel events mPackageId tagUuids = do
  mKm <- getFromCache events mPackageId tagUuids
  case mKm of
    Just km -> return km
    Nothing -> do
      km <- compileFromScratch
      addToCache events mPackageId tagUuids km
      return km
  where
    compileFromScratch :: AppContextM KnowledgeModel
    compileFromScratch = do
      allEvents <- getEvents mPackageId
      km <- liftEither $ compile Nothing allEvents
      return $ filterKnowledgeModel tagUuids km
    getEvents :: Maybe String -> AppContextM [Event]
    getEvents Nothing = return events
    getEvents (Just packageId) = do
      eventsFromPackage <- getAllPreviousEventsSincePackageId packageId
      return $ eventsFromPackage ++ events
