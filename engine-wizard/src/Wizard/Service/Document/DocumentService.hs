module Wizard.Service.Document.DocumentService where

import Control.Lens ((^.), (^?), _Just)
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
import Shared.Model.Common.Lens
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Error.Error
import Shared.Util.List
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Document.DocumentQueueDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentQueue
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Document.DocumentAcl
import Wizard.Service.Document.DocumentContextService
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Document.DocumentUtils
import Wizard.Service.Limit.AppLimitService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Template.TemplateService
import Wizard.Service.Template.TemplateValidation
import Wizard.Util.Logger

getDocumentsPageDto :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsPageDto mQuestionnaireUuid mQuery pageable sort =
  runInTransaction $ do
    checkPermission _DOC_PERM
    docPage <- findDocumentsPage mQuestionnaireUuid mQuery pageable sort
    traverse enhanceDocument docPage

getDocumentsForQuestionnaire :: String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsForQuestionnaire qtnUuid mQuery pageable sort =
  runInTransaction $ do
    checkViewPermissionToDoc qtnUuid
    docPage <- findDocumentsByQuestionnaireUuidPage qtnUuid mQuery pageable sort
    traverse enhanceDocument docPage

createDocument :: DocumentCreateDTO -> AppContextM DocumentDTO
createDocument reqDto =
  runInTransaction $ do
    checkEditPermissionToDoc (U.toString $ reqDto ^. questionnaireUuid)
    createDocumentWithDurability reqDto PersistentDocumentDurability

createDocumentWithDurability :: DocumentCreateDTO -> DocumentDurability -> AppContextM DocumentDTO
createDocumentWithDurability dto durability =
  runInTransaction $ do
    checkDocumentLimit
    qtnSimple <- findQuestionnaireSimpleById (U.toString $ dto ^. questionnaireUuid)
    qtn <- findQuestionnaireById (U.toString $ dto ^. questionnaireUuid)
    tml <- getTemplateByUuidAndPackageId (dto ^. templateId) (Just $ qtn ^. packageId)
    validateMetamodelVersion tml
    mCurrentUser <- asks _appContextCurrentUser
    dUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    appUuid <- asks _appContextAppUuid
    let qtnEvents =
          case dto ^. questionnaireEventUuid of
            Just eventUuid -> takeWhileInclusive (\e -> e ^. uuid' /= eventUuid) (qtn ^. events)
            Nothing -> qtn ^. events
    qtnCtn <- compileQuestionnairePreview qtnEvents
    let repliesHash = computeHash qtnCtn
    let doc = fromCreateDTO dto dUuid durability repliesHash mCurrentUser appUuid now
    insertDocument doc
    publishToDocumentQueue doc
    return $ toDTO doc (Just qtnSimple) [] tml

deleteDocument :: String -> AppContextM ()
deleteDocument docUuid =
  runInTransaction $ do
    doc <- findDocumentById docUuid
    checkEditPermissionToDoc (U.toString $ doc ^. questionnaireUuid)
    deleteSubmissionsFiltered [("document_uuid", docUuid)]
    deleteDocumentById docUuid
    removeDocumentContent docUuid

downloadDocument :: String -> AppContextM (Document, BS.ByteString)
downloadDocument docUuid =
  runInTransaction $ do
    doc <- findDocumentById docUuid
    content <- getDocumentContent docUuid
    return (doc, content)

cleanDocuments :: AppContextM ()
cleanDocuments =
  runInTransaction $ do
    docs <- findDocumentsFiltered [("durability", "TemporallyDocumentDurability")]
    let docsFiltered = filter (\d -> d ^. state == DoneDocumentState || d ^. state == ErrorDocumentState) docs
    traverse_
      (\d -> do
         deleteDocumentsFiltered [("uuid", U.toString $ d ^. uuid)]
         removeDocumentContent (U.toString $ d ^. uuid))
      docsFiltered

createDocumentPreview :: String -> AppContextM (Document, BS.ByteString)
createDocumentPreview qtnUuid =
  runInTransaction $ do
    qtn <- findQuestionnaireById qtnUuid
    checkViewPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
    docs <- findDocumentsFiltered [("questionnaire_uuid", qtnUuid), ("durability", "TemporallyDocumentDurability")]
    qtnCtn <- compileQuestionnaire qtn
    let repliesHash = computeHash qtnCtn
    logDebugU _CMP_SERVICE ("Replies hash: " ++ show repliesHash)
    let matchingDocs = filter (\d -> d ^. questionnaireRepliesHash == repliesHash) docs
    case filter (filterAlreadyDone qtn) matchingDocs of
      (doc:_) -> do
        logInfoU _CMP_SERVICE "Retrieving from cache"
        if doc ^. state == DoneDocumentState
          then do
            content <- getDocumentContent (U.toString $ doc ^. uuid)
            return (doc, content)
          else return (doc, BS.empty)
      [] ->
        case filter (\d -> d ^. state == QueuedDocumentState || d ^. state == InProgressDocumentState) matchingDocs of
          (doc:_) -> do
            logInfoU _CMP_SERVICE "Waiting to generation"
            return (doc, BS.empty)
          _ -> createNewDoc qtn
  where
    filterAlreadyDone :: Questionnaire -> Document -> Bool
    filterAlreadyDone qtn doc =
      (doc ^. state == DoneDocumentState || doc ^. state == ErrorDocumentState) && Just (doc ^. templateId) == qtn ^.
      templateId &&
      Just (doc ^. formatUuid) ==
      qtn ^.
      formatUuid
    createNewDoc :: Questionnaire -> AppContextM (Document, BS.ByteString)
    createNewDoc qtn = do
      logInfoU _CMP_SERVICE "Generating new preview"
      case (qtn ^. templateId, qtn ^. formatUuid) of
        (Just tUuid, Just fUuid) -> do
          let reqDto =
                DocumentCreateDTO
                  { _documentCreateDTOName = qtn ^. name
                  , _documentCreateDTOQuestionnaireUuid = qtn ^. uuid
                  , _documentCreateDTOQuestionnaireEventUuid = lastSafe (qtn ^. events) ^? _Just . uuid'
                  , _documentCreateDTOTemplateId = tUuid
                  , _documentCreateDTOFormatUuid = fUuid
                  }
          docDto <- createDocumentWithDurability reqDto TemporallyDocumentDurability
          doc <- findDocumentById (U.toString $ docDto ^. uuid)
          return (doc, BS.empty)
        _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP

publishToDocumentQueue :: Document -> AppContextM ()
publishToDocumentQueue doc = do
  now <- liftIO getCurrentTime
  appUuid <- asks _appContextAppUuid
  docContext <- createDocumentContext doc
  let dq =
        DocumentQueue
          { _documentQueueDId = 0
          , _documentQueueDocumentUuid = doc ^. uuid
          , _documentQueueDocumentContext = docContext
          , _documentQueueAppUuid = appUuid
          , _documentQueueCreatedBy = doc ^. creatorUuid
          , _documentQueueCreatedAt = now
          }
  dId <- insertDocumentQueue dq
  notifyDocumentQueue dId
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
computeHash :: QuestionnaireContent -> Int
computeHash qtnCtn = (hash . M.toList $ qtnCtn ^. replies) + maybe 0 hash (qtnCtn ^. phaseUuid)
