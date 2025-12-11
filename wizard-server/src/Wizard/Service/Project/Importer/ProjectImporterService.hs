module Wizard.Service.Project.Importer.ProjectImporterService where

import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Api.Resource.Project.Importer.ProjectImporterChangeDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectImporterDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.Importer.ProjectImporter
import Wizard.Model.Project.Project
import Wizard.Service.Project.Importer.ProjectImporterAudit
import Wizard.Service.Project.Importer.ProjectImporterMapper
import Wizard.Service.Project.Importer.ProjectImporterUtil

getProjectImportersPageDto :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page ProjectImporterDTO)
getProjectImportersPageDto mQuery pageable sort = do
  checkPermission _PRJ_PERM
  currentUser <- getCurrentUser
  importersPage <- findProjectImportersPage Nothing Nothing mQuery Nothing pageable sort
  return $ fmap toDTO importersPage

getProjectImporterSuggestions :: Maybe U.UUID -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page ProjectImporterDTO)
getProjectImporterSuggestions mProjectUuid mQuery mEnabled pageable sort = do
  checkPermission _PRJ_PERM
  mPkgId <-
    case mProjectUuid of
      Just projectUuid -> do
        project <- findProjectByUuid projectUuid
        return . Just $ project.knowledgeModelPackageId
      Nothing -> return Nothing
  page <- findProjectImportersPage Nothing Nothing mQuery mEnabled (Pageable (Just 0) (Just 999999999)) sort
  return . fmap toDTO . updatePage page . filterImportersInGroup mPkgId $ page
  where
    updatePage :: Page ProjectImporter -> [ProjectImporter] -> Page ProjectImporter
    updatePage (Page name _ _) array =
      let updatedArray = take updatedSize array
          updatedSize = fromMaybe 20 pageable.size
          updatedTotalElements = length updatedArray
          updatedTotalPages = computeTotalPage updatedTotalElements updatedSize
          updatedNumber = fromMaybe 0 pageable.page
       in Page name (PageMetadata updatedSize updatedTotalElements updatedTotalPages updatedNumber) updatedArray
    filterImportersInGroup :: Maybe String -> Page ProjectImporter -> [ProjectImporter]
    filterImportersInGroup mPkgId page =
      filter isProjectImporterSupported . filterProjectImporters mPkgId $ page.entities

getProjectImporter :: String -> AppContextM ProjectImporterDTO
getProjectImporter piId =
  runInTransaction $ do
    checkPermission _PRJ_PERM
    importer <- findProjectImporterById piId
    auditProjectImporterStartEvent piId
    return $ toDTO importer

modifyProjectImporter :: String -> ProjectImporterChangeDTO -> AppContextM ProjectImporterDTO
modifyProjectImporter piId reqDto =
  runInTransaction $ do
    checkPermission _PRJ_IMPORTER_PERM
    importer <- findProjectImporterById piId
    now <- liftIO getCurrentTime
    let updatedImporter = fromChangeDTO importer reqDto now
    updateProjectImporterById updatedImporter
    return $ toDTO updatedImporter
