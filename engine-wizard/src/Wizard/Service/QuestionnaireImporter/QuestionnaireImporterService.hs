module Wizard.Service.QuestionnaireImporter.QuestionnaireImporterService where

import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.QuestionnaireImporter.QuestionnaireImporterDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter
import Wizard.Service.Acl.AclService
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterAudit
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterUtil

getQuestionnaireImportersPageDto :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireImporterDTO)
getQuestionnaireImportersPageDto mQuery pageable sort = do
  checkPermission _QTN_PERM
  currentUser <- getCurrentUser
  importersPage <- findQuestionnaireImportersPage Nothing Nothing mQuery Nothing pageable sort
  return $ fmap toDTO importersPage

getQuestionnaireImporterSuggestions
  :: Maybe U.UUID -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireImporterDTO)
getQuestionnaireImporterSuggestions mQuestionnaireUuid mQuery mEnabled pageable sort = do
  checkPermission _QTN_PERM
  mPkgId <-
    case mQuestionnaireUuid of
      Just qtnUuid -> do
        qtn <- findQuestionnaireByUuid qtnUuid
        return . Just $ qtn.packageId
      Nothing -> return Nothing
  page <- findQuestionnaireImportersPage Nothing Nothing mQuery mEnabled (Pageable (Just 0) (Just 999999999)) sort
  return . fmap toDTO . updatePage page . filterImportersInGroup mPkgId $ page
  where
    updatePage :: Page QuestionnaireImporter -> [QuestionnaireImporter] -> Page QuestionnaireImporter
    updatePage (Page name _ _) array =
      let updatedArray = take updatedSize array
          updatedSize = fromMaybe 20 pageable.size
          updatedTotalElements = length updatedArray
          updatedTotalPages = computeTotalPage updatedTotalElements updatedSize
          updatedNumber = fromMaybe 0 pageable.page
       in Page name (PageMetadata updatedSize updatedTotalElements updatedTotalPages updatedNumber) updatedArray
    filterImportersInGroup :: Maybe String -> Page QuestionnaireImporter -> [QuestionnaireImporter]
    filterImportersInGroup mPkgId page =
      filter isQuestionnaireImporterSupported . filterQuestionnaireImporters mPkgId $ page.entities

getQuestionnaireImporter :: String -> AppContextM QuestionnaireImporterDTO
getQuestionnaireImporter qiId =
  runInTransaction $ do
    checkPermission _QTN_PERM
    importer <- findQuestionnaireImporterById qiId
    auditQuestionnaireImporterStartEvent qiId
    return $ toDTO importer

modifyQuestionnaireImporter :: String -> QuestionnaireImporterChangeDTO -> AppContextM QuestionnaireImporterDTO
modifyQuestionnaireImporter qiId reqDto =
  runInTransaction $ do
    checkPermission _QTN_IMPORTER_PERM
    importer <- findQuestionnaireImporterById qiId
    now <- liftIO getCurrentTime
    let updatedImporter = fromChangeDTO importer reqDto now
    updateQuestionnaireImporterById updatedImporter
    return $ toDTO updatedImporter
