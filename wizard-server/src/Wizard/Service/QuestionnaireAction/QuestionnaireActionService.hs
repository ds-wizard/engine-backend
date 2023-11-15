module Wizard.Service.QuestionnaireAction.QuestionnaireActionService where

import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.QuestionnaireAction.QuestionnaireActionDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.QuestionnaireAction.QuestionnaireAction
import Wizard.Service.QuestionnaireAction.QuestionnaireActionAudit
import Wizard.Service.QuestionnaireAction.QuestionnaireActionMapper
import Wizard.Service.QuestionnaireAction.QuestionnaireActionUtil

getQuestionnaireActionsPageDto :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireActionDTO)
getQuestionnaireActionsPageDto mQuery pageable sort = do
  checkPermission _QTN_PERM
  currentUser <- getCurrentUser
  importersPage <- findQuestionnaireActionsPage Nothing Nothing mQuery Nothing pageable sort
  return $ fmap toDTO importersPage

getQuestionnaireActionSuggestions
  :: Maybe U.UUID -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireActionDTO)
getQuestionnaireActionSuggestions mQuestionnaireUuid mQuery mEnabled pageable sort = do
  checkPermission _QTN_PERM
  mPkgId <-
    case mQuestionnaireUuid of
      Just qtnUuid -> do
        qtn <- findQuestionnaireByUuid qtnUuid
        return . Just $ qtn.packageId
      Nothing -> return Nothing
  page <- findQuestionnaireActionsPage Nothing Nothing mQuery mEnabled (Pageable (Just 0) (Just 999999999)) sort
  return . fmap toDTO . updatePage page . filterImportersInGroup mPkgId $ page
  where
    updatePage :: Page QuestionnaireAction -> [QuestionnaireAction] -> Page QuestionnaireAction
    updatePage (Page name _ _) array =
      let updatedArray = take updatedSize array
          updatedSize = fromMaybe 20 pageable.size
          updatedTotalElements = length updatedArray
          updatedTotalPages = computeTotalPage updatedTotalElements updatedSize
          updatedNumber = fromMaybe 0 pageable.page
       in Page name (PageMetadata updatedSize updatedTotalElements updatedTotalPages updatedNumber) updatedArray
    filterImportersInGroup :: Maybe String -> Page QuestionnaireAction -> [QuestionnaireAction]
    filterImportersInGroup mPkgId page =
      filter isQuestionnaireActionSupported . filterQuestionnaireActions mPkgId $ page.entities

getQuestionnaireAction :: String -> AppContextM QuestionnaireActionDTO
getQuestionnaireAction qiId =
  runInTransaction $ do
    checkPermission _QTN_PERM
    importer <- findQuestionnaireActionById qiId
    auditQuestionnaireActionStartEvent qiId
    return $ toDTO importer

modifyQuestionnaireAction :: String -> QuestionnaireActionChangeDTO -> AppContextM QuestionnaireActionDTO
modifyQuestionnaireAction qiId reqDto =
  runInTransaction $ do
    checkPermission _QTN_ACTION_PERM
    importer <- findQuestionnaireActionById qiId
    now <- liftIO getCurrentTime
    let updatedImporter = fromChangeDTO importer reqDto now
    updateQuestionnaireActionById updatedImporter
    return $ toDTO updatedImporter
