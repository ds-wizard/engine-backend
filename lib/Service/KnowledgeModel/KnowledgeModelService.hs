module Service.KnowledgeModel.KnowledgeModelService where

import Control.Lens ((^.))
import qualified Data.UUID as U

import Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelFilter
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Migration.KnowledgeModel.Applicator.Applicator
import Service.Package.PackageService

createKnowledgeModelPreview :: KnowledgeModelChangeDTO -> AppContextM (Either AppError KnowledgeModelDTO)
createKnowledgeModelPreview reqDto = do
  km <- compileKnowledgeModel (fromDTOs $ reqDto ^. events) (reqDto ^. packageId) (reqDto ^. tagUuids)
  return . fmap toKnowledgeModelDTO $ km

compileKnowledgeModel :: [Event] -> Maybe String -> [U.UUID] -> AppContextM (Either AppError KnowledgeModel)
compileKnowledgeModel events mPackageId tagUuids =
  getEvents mPackageId $ \allEvents -> return . fmap (filterKnowledgeModel tagUuids) . runApplicator Nothing $ allEvents
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
