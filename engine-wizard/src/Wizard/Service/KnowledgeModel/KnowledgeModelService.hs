module Wizard.Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))
import Control.Monad.Except (liftEither)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.Event.EventMapper
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.KnowledgeModel.KnowledgeModelFilter
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Service.Package.PackageService

createKnowledgeModelPreview :: KnowledgeModelChangeDTO -> AppContextM KnowledgeModelDTO
createKnowledgeModelPreview reqDto = do
  km <- compileKnowledgeModel (fromDTOs $ reqDto ^. events) (reqDto ^. packageId) (reqDto ^. tagUuids)
  return $ toKnowledgeModelDTO km

compileKnowledgeModel :: [Event] -> Maybe String -> [U.UUID] -> AppContextM KnowledgeModel
compileKnowledgeModel events mPackageId tagUuids = do
  allEvents <- getEvents mPackageId
  km <- liftEither $ compile Nothing allEvents
  return $ filterKnowledgeModel tagUuids km
  where
    getEvents Nothing = return events
    getEvents (Just packageId) = do
      eventsFromPackage <- getAllPreviousEventsSincePackageId packageId
      return $ eventsFromPackage ++ events
