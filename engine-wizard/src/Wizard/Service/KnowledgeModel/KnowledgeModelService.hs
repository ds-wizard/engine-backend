module Wizard.Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))
import qualified Data.UUID as U

import Shared.Model.Error.Error
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Wizard.LensesConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Event.EventMapper
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.KnowledgeModel.KnowledgeModelFilter
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Service.Package.PackageService

createKnowledgeModelPreview :: KnowledgeModelChangeDTO -> AppContextM (Either AppError KnowledgeModelDTO)
createKnowledgeModelPreview reqDto = do
  km <- compileKnowledgeModel (fromDTOs $ reqDto ^. events) (reqDto ^. packageId) (reqDto ^. tagUuids)
  return . fmap toKnowledgeModelDTO $ km

compileKnowledgeModel :: [Event] -> Maybe String -> [U.UUID] -> AppContextM (Either AppError KnowledgeModel)
compileKnowledgeModel events mPackageId tagUuids =
  getEvents mPackageId $ \allEvents -> return . fmap (filterKnowledgeModel tagUuids) . compile Nothing $ allEvents
  where
    getEvents Nothing callback = callback events
    getEvents (Just packageId) callback =
      heGetAllPreviousEventsSincePackageId packageId $ \eventsFromPackage -> callback $ eventsFromPackage ++ events

-- --------------------------------
-- HELPERS
-- --------------------------------
heCompileKnowledgeModel events mPackageId tagUuids callback = do
  eitherResult <- compileKnowledgeModel events mPackageId tagUuids
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error

hmCompileKnowledgeModel events mPackageId tagUuids callback = do
  eitherResult <- compileKnowledgeModel events mPackageId tagUuids
  case eitherResult of
    Right result -> callback result
    Left error -> return . Just $ error
