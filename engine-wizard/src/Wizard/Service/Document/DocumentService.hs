module Wizard.Service.Document.DocumentService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig hiding (hash)
import Shared.Constant.Component
import Shared.Localization.Messages.Public
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
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User
import Wizard.Service.Common.ACL
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Document.DocumentUtils
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Template.TemplateService
import Wizard.Service.Template.TemplateValidation
import Wizard.Util.Logger

getDocumentsForCurrentUserPageDto ::
     Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsForCurrentUserPageDto mQuestionnaireUuid mQuery pageable sort = do
  checkPermission _DMP_PERM
  docPage <- findDocumentsForCurrentUserPage mQuestionnaireUuid mQuery pageable sort
  traverse enhanceDocument docPage

createDocument :: DocumentCreateDTO -> AppContextM DocumentDTO
createDocument reqDto = createDocumentWithDurability reqDto PersistentDocumentDurability

createDocumentWithDurability :: DocumentCreateDTO -> DocumentDurability -> AppContextM DocumentDTO
createDocumentWithDurability dto durability = do
  checkPermission _DMP_PERM
  qtnDto <- getQuestionnaireById (U.toString $ dto ^. questionnaireUuid)
  qtn <- findQuestionnaireById (U.toString $ dto ^. questionnaireUuid)
  tml <- getTemplateByUuid (dto ^. templateId) (Just $ qtn ^. packageId)
  validateMetamodelVersion tml
  currentUser <- getCurrentUser
  dUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let repliesHash = hash . M.toList $ qtn ^. replies
  let doc = fromCreateDTO dto dUuid durability repliesHash (currentUser ^. uuid) now
  insertDocument doc
  publishToDocumentQueue doc
  return $ toDTO doc (Just qtnDto) tml

deleteDocument :: String -> AppContextM ()
deleteDocument docUuid = do
  checkPermission _DMP_PERM
  currentUser <- getCurrentUser
  doc <- findDocumentById docUuid
  if currentUser ^. role == _USER_ROLE_ADMIN || currentUser ^. uuid == doc ^. ownerUuid
    then do
      deleteDocumentById docUuid
      deleteDocumentContentsFiltered [("filename", docUuid)]
      return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Delete Document"

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

createPreview :: String -> AppContextM (Document, BS.ByteString)
createPreview qtnUuid = do
  checkPermission _QTN_PERM
  qtn <- findQuestionnaireById qtnUuid
  docs <- findDocumentsFiltered [("questionnaireUuid", qtnUuid), ("durability", "TemporallyDocumentDurability")]
  let repliesHash = hash . M.toList $ qtn ^. replies
  logDebugU _CMP_SERVICE ("Replies hash: " ++ show repliesHash)
  let matchingDocs = filter (\d -> d ^. questionnaireRepliesHash == repliesHash) docs
  case filter (\d -> d ^. state == DoneDocumentState) matchingDocs of
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

checkPermissionToDocument :: String -> AppContextM ()
checkPermissionToDocument docUuid = do
  currentUser <- getCurrentUser
  doc <- findDocumentById docUuid
  if (currentUser ^. role == _USER_ROLE_ADMIN) || (currentUser ^. uuid == doc ^. ownerUuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Detail Document"
