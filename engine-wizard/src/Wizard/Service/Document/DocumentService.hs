module Wizard.Service.Document.DocumentService where

import Control.Lens ((^.))
import Control.Monad (forM)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import Data.Time
import qualified Data.UUID as U

import LensesConfig hiding (hash)
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Constant.Component
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Messaging.Out.Queue.Document
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Template.TemplateService
import Wizard.Util.Logger

getDocumentsFiltered :: [(String, String)] -> AppContextM [DocumentDTO]
getDocumentsFiltered queryParams = do
  currentUser <- getCurrentUser
  documents <- _getDocuments currentUser
  forM documents enhance
  where
    _getDocuments currentUser =
      if currentUser ^. role == _USER_ROLE_ADMIN
        then findDocumentsFiltered queryParams
        else findDocumentsFiltered (queryParams ++ [("ownerUuid", U.toString $ currentUser ^. uuid)])
    enhance :: Document -> AppContextM DocumentDTO
    enhance doc = do
      tml <- getTemplateByUuid (U.toString $ doc ^. templateUuid) Nothing
      mQtn <- catchError (getQuestionnaireById' (U.toString $ doc ^. questionnaireUuid)) (\_ -> return Nothing)
      return $ toDTO doc mQtn tml

createDocument :: DocumentCreateDTO -> AppContextM DocumentDTO
createDocument reqDto = createDocumentWithDurability reqDto PersistentDocumentDurability

createDocumentWithDurability :: DocumentCreateDTO -> DocumentDurability -> AppContextM DocumentDTO
createDocumentWithDurability dto durability = do
  qtnDto <- getQuestionnaireById (U.toString $ dto ^. questionnaireUuid)
  qtn <- findQuestionnaireById (U.toString $ dto ^. questionnaireUuid)
  tml <- getTemplateByUuid (U.toString $ dto ^. templateUuid) (Just $ qtn ^. packageId)
  currentUser <- getCurrentUser
  dUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let repliesHash = hash (qtn ^. replies)
  let doc = fromCreateDTO dto dUuid durability repliesHash (currentUser ^. uuid) now
  insertDocument doc
  publishToDocumentQueue doc
  return $ toDTO doc (Just qtnDto) tml

deleteDocument :: String -> AppContextM ()
deleteDocument docUuid = do
  currentUser <- getCurrentUser
  doc <- findDocumentById docUuid
  if currentUser ^. role == _USER_ROLE_ADMIN || currentUser ^. uuid == doc ^. ownerUuid
    then do
      deleteDocumentById docUuid
      deleteDocumentContentById docUuid
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
  forM
    docsFiltered
    (\d -> do
       deleteDocumentsFiltered [("uuid", U.toString $ d ^. uuid)]
       deleteDocumentContentsFiltered [("filename", U.toString $ d ^. uuid)])
  return ()

createPreview :: String -> AppContextM (Document, BS.ByteString)
createPreview qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  docs <- findDocumentsFiltered [("questionnaireUuid", qtnUuid), ("durability", "TemporallyDocumentDurability")]
  let repliesHash = hash (qtn ^. replies)
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
      case (qtn ^. templateUuid, qtn ^. formatUuid) of
        (Just tUuid, Just fUuid) -> do
          let reqDto =
                DocumentCreateDTO
                  { _documentCreateDTOName = qtn ^. name
                  , _documentCreateDTOQuestionnaireUuid = qtn ^. uuid
                  , _documentCreateDTOTemplateUuid = tUuid
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
