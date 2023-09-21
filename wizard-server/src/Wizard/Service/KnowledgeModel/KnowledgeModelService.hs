module Wizard.Service.KnowledgeModel.KnowledgeModelService where

import Control.Monad.Except (liftEither)
import qualified Data.UUID as U

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
compileKnowledgeModel events mPackageId tagUuids = do
  mResolvedPackageId <- traverse resolvePackageId mPackageId
  allEvents <- getEvents mPackageId
  km <- liftEither $ compile Nothing allEvents
  return $ filterKnowledgeModel tagUuids km
  where
    getEvents :: Maybe String -> AppContextM [Event]
    getEvents Nothing = return events
    getEvents (Just packageId) = do
      pkgs <- findSeriesOfPackagesRecursiveById packageId :: AppContextM [PackageWithEvents]
      return . concatMap (.events) $ pkgs
