module Wizard.Service.KnowledgeModel.KnowledgeModelService where

import Control.Monad.Except (liftEither)
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.Package.PackageUtil
import Wizard.Model.Context.AppContext
import Wizard.Service.Acl.AclService
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.KnowledgeModel.KnowledgeModelFilter
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageUtil

createKnowledgeModelPreview :: KnowledgeModelChangeDTO -> AppContextM KnowledgeModel
createKnowledgeModelPreview reqDto = do
  mResolvedPackageId <- traverse resolvePackageId reqDto.packageId
  checkIfPackageIsPublic mResolvedPackageId _QTN_PERM
  compileKnowledgeModel reqDto.events mResolvedPackageId reqDto.tagUuids

compileKnowledgeModel :: [Event] -> Maybe String -> [U.UUID] -> AppContextM KnowledgeModel
compileKnowledgeModel events mPackageId tagUuids = do
  mResolvedPackageId <- traverse resolvePackageId mPackageId
  allEvents <- getEvents mPackageId
  km <- liftEither $ compile Nothing allEvents
  return $ filterKnowledgeModel tagUuids km
  where
    getEvents :: Maybe String -> AppContextM [Event]
    getEvents Nothing = return events
    getEvents (Just packageId) = do
      eventsFromPackage <- getAllPreviousEventsSincePackageId packageId
      return $ eventsFromPackage ++ events
