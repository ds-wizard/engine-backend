module Wizard.Service.Project.Action.ProjectActionService where

import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Api.Resource.Project.Action.ProjectActionChangeDTO
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectActionDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.Action.ProjectAction
import Wizard.Model.Project.Project
import Wizard.Service.Project.Action.ProjectActionAudit
import Wizard.Service.Project.Action.ProjectActionMapper
import Wizard.Service.Project.Action.ProjectActionUtil

getProjectActionsPageDto :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page ProjectActionDTO)
getProjectActionsPageDto mQuery pageable sort = do
  checkPermission _PRJ_PERM
  currentUser <- getCurrentUser
  importersPage <- findProjectActionsPage Nothing Nothing mQuery Nothing pageable sort
  return $ fmap toDTO importersPage

getProjectActionSuggestions
  :: Maybe U.UUID -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page ProjectActionDTO)
getProjectActionSuggestions mProjectUuid mQuery mEnabled pageable sort = do
  checkPermission _PRJ_PERM
  mPkgId <-
    case mProjectUuid of
      Just projectUuid -> do
        project <- findProjectByUuid projectUuid
        return . Just $ project.knowledgeModelPackageId
      Nothing -> return Nothing
  page <- findProjectActionsPage Nothing Nothing mQuery mEnabled (Pageable (Just 0) (Just 999999999)) sort
  return . fmap toDTO . updatePage page . filterImportersInGroup mPkgId $ page
  where
    updatePage :: Page ProjectAction -> [ProjectAction] -> Page ProjectAction
    updatePage (Page name _ _) array =
      let updatedArray = take updatedSize array
          updatedSize = fromMaybe 20 pageable.size
          updatedTotalElements = length updatedArray
          updatedTotalPages = computeTotalPage updatedTotalElements updatedSize
          updatedNumber = fromMaybe 0 pageable.page
       in Page name (PageMetadata updatedSize updatedTotalElements updatedTotalPages updatedNumber) updatedArray
    filterImportersInGroup :: Maybe String -> Page ProjectAction -> [ProjectAction]
    filterImportersInGroup mPkgId page =
      filter isProjectActionSupported . filterProjectActions mPkgId $ page.entities

getProjectAction :: String -> AppContextM ProjectActionDTO
getProjectAction piId =
  runInTransaction $ do
    checkPermission _PRJ_PERM
    importer <- findProjectActionById piId
    auditProjectActionStartEvent piId
    return $ toDTO importer

modifyProjectAction :: String -> ProjectActionChangeDTO -> AppContextM ProjectActionDTO
modifyProjectAction piId reqDto =
  runInTransaction $ do
    checkPermission _PRJ_ACTION_PERM
    importer <- findProjectActionById piId
    now <- liftIO getCurrentTime
    let updatedImporter = fromChangeDTO importer reqDto now
    updateProjectActionById updatedImporter
    return $ toDTO updatedImporter
