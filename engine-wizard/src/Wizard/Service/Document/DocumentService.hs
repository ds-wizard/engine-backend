module Wizard.Service.Document.DocumentService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig hiding (hash)
import Shared.Constant.Component
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Messaging.Out.Queue.Document
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Common.ACL
import Wizard.Service.Document.DocumentACL
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Document.DocumentUtils
import Wizard.Service.Questionnaire.QuestionnaireACL
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Template.TemplateService
import Wizard.Service.Template.TemplateValidation
import Wizard.Util.Logger

getDocumentsPageDto :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsPageDto mQuestionnaireUuid mQuery pageable sort = do
  checkRole _USER_ROLE_ADMIN
  docPage <- findDocumentsPage mQuestionnaireUuid mQuery pageable sort
  traverse enhanceDocument docPage

getDocumentsForQuestionnaire :: String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsForQuestionnaire qtnUuid mQuery pageable sort = do
  checkViewPermissionToDoc qtnUuid
  docPage <- findDocumentsByQuestionnaireUuidPage qtnUuid mQuery pageable sort
  traverse enhanceDocument docPage

createDocument :: DocumentCreateDTO -> AppContextM DocumentDTO
createDocument reqDto = do
  checkEditPermissionToDoc (U.toString $ reqDto ^. questionnaireUuid)
  createDocumentWithDurability reqDto PersistentDocumentDurability

createDocumentWithDurability :: DocumentCreateDTO -> DocumentDurability -> AppContextM DocumentDTO
createDocumentWithDurability dto durability = do
  qtnDto <- getQuestionnaireById (U.toString $ dto ^. questionnaireUuid)
  qtn <- findQuestionnaireById (U.toString $ dto ^. questionnaireUuid)
  tml <- getTemplateByUuid (dto ^. templateId) (Just $ qtn ^. packageId)
  validateMetamodelVersion tml
  mCurrentUser <- asks _appContextCurrentUser
  dUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let repliesHash = hash . M.toList $ qtn ^. replies
  let doc = fromCreateDTO dto dUuid durability repliesHash mCurrentUser now
  insertDocument doc
  publishToDocumentQueue doc
  return $ toDTO doc (Just qtnDto) tml

deleteDocument :: String -> AppContextM ()
deleteDocument docUuid = do
  doc <- findDocumentById docUuid
  checkEditPermissionToDoc (U.toString $ doc ^. questionnaireUuid)
  deleteDocumentById docUuid
  deleteDocumentContentsFiltered [("filename", docUuid)]

downloadDocument :: String -> AppContextM (Document, BS.ByteString)
downloadDocument docUuid = do
  doc <- findDocumentById docUuid
  content <- findDocumentContent docUuid
  return (doc, content)

cleanDocuments :: AppContextM ()
cleanDocuments = do
  docs <- findDocumentsFiltered [("durability", "TemporallyDocumentDurability")]
  let docsFiltered = filter (\d -> d ^. state == DoneDocumentState || d ^. state == ErrorDocumentState) docs
  traverse_
    (\d -> do
       deleteDocumentsFiltered [("uuid", U.toString $ d ^. uuid)]
       deleteDocumentContentsFiltered [("filename", U.toString $ d ^. uuid)])
    docsFiltered

createDocumentPreview :: String -> AppContextM (Document, BS.ByteString)
createDocumentPreview qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkViewPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. ownerUuid)
  docs <- findDocumentsFiltered [("questionnaireUuid", qtnUuid), ("durability", "TemporallyDocumentDurability")]
  let repliesHash = hash . M.toList $ qtn ^. replies
  logDebugU _CMP_SERVICE ("Replies hash: " ++ show repliesHash)
  let matchingDocs = filter (\d -> d ^. questionnaireRepliesHash == repliesHash) docs
  case filter (filterAlreadyDone qtn) matchingDocs of
    (doc:_) -> do
      logInfoU _CMP_SERVICE "Retrieving from cache"
      content <- findDocumentContent (U.toString $ doc ^. uuid)
      return (doc, content)
    [] ->
      case filter (\d -> d ^. state == QueuedDocumentState || d ^. state == InProgressDocumentState) matchingDocs of
        (doc:_) -> do
          logInfoU _CMP_SERVICE "Waiting to generation"
          return (doc, BS.empty)
        _ -> createNewDoc qtn
  where
    filterAlreadyDone :: Questionnaire -> Document -> Bool
    filterAlreadyDone qtn doc =
      doc ^. state == DoneDocumentState &&
      Just (doc ^. templateId) == qtn ^. templateId && Just (doc ^. formatUuid) == qtn ^. formatUuid
    createNewDoc :: Questionnaire -> AppContextM (Document, BS.ByteString)
    createNewDoc qtn = do
      logInfoU _CMP_SERVICE "Generating new preview"
      case (qtn ^. templateId, qtn ^. formatUuid) of
        (Just tUuid, Just fUuid) -> do
          let reqDto =
                DocumentCreateDTO
                  { _documentCreateDTOName = qtn ^. name
                  , _documentCreateDTOQuestionnaireUuid = qtn ^. uuid
                  , _documentCreateDTOTemplateId = tUuid
                  , _documentCreateDTOFormatUuid = fUuid
                  }
          docDto <- createDocumentWithDurability reqDto TemporallyDocumentDurability
          doc <- findDocumentById (U.toString $ docDto ^. uuid)
          return (doc, BS.empty)
        _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP
