module Wizard.Service.Document.DocumentService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.Component
import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.Common.Util.List
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.TemporaryFile.TemporaryFileDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Document.Context.DocumentContextService
import Wizard.Service.Document.DocumentAcl
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Document.DocumentUtil
import Wizard.Service.DocumentTemplate.DocumentTemplateService
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import Wizard.Service.Limit.AppLimitService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import qualified Wizard.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Wizard.Service.TemporaryFile.TemporaryFileService
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

getDocumentsPageDto :: Maybe U.UUID -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsPageDto mQuestionnaireUuid mQuery pageable sort = do
  checkPermission _DOC_PERM
  docPage <- findDocumentsPage mQuestionnaireUuid mQuery pageable sort
  traverse enhanceDocument docPage

getDocumentsForQtn :: U.UUID -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsForQtn qtnUuid mQuery pageable sort = do
  checkViewPermissionToDoc qtnUuid
  docPage <- findDocumentsByQuestionnaireUuidPage qtnUuid mQuery pageable sort
  traverse enhanceDocument docPage

createDocument :: DocumentCreateDTO -> AppContextM DocumentDTO
createDocument reqDto =
  runInTransaction $ do
    checkEditPermissionToDoc reqDto.questionnaireUuid
    checkDocumentLimit
    qtnSimple <- findQuestionnaireSimpleByUuid reqDto.questionnaireUuid
    qtn <- findQuestionnaireByUuid reqDto.questionnaireUuid
    tml <- getDocumentTemplateByUuidAndPackageId reqDto.documentTemplateId (Just qtn.packageId)
    validateMetamodelVersion tml
    mCurrentUser <- asks currentUser
    dUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    appUuid <- asks currentAppUuid
    let qtnEvents =
          case reqDto.questionnaireEventUuid of
            Just eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) qtn.events
            Nothing -> qtn.events
    qtnCtn <- compileQuestionnairePreview qtnEvents
    appConfig <- getAppConfig
    let repliesHash = computeHash qtn qtnCtn appConfig mCurrentUser
    let doc = fromCreateDTO reqDto dUuid repliesHash qtn.events mCurrentUser appUuid now
    insertDocument doc
    publishToPersistentCommandQueue doc
    return $ toDTO doc (Just qtnSimple) [] tml

deleteDocument :: U.UUID -> AppContextM ()
deleteDocument docUuid =
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkEditPermissionToDoc doc.questionnaireUuid
    deleteSubmissionsFiltered [("document_uuid", U.toString docUuid)]
    deleteDocumentByUuid docUuid
    removeDocumentContent docUuid

downloadDocument :: U.UUID -> AppContextM TemporaryFileDTO
downloadDocument docUuid = do
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkViewPermissionToDoc doc.questionnaireUuid
    content <- retrieveDocumentContent docUuid
    let fileName = fromMaybe "export" doc.fileName
    let contentType = fromMaybe "text/plain" doc.contentType
    url <- createTemporaryFile fileName "application/octet-stream" (BSL.fromStrict content)
    return $ TemporaryFileMapper.toDTO url contentType

createDocumentPreviewForQtn :: U.UUID -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreviewForQtn qtnUuid =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    case (qtn.documentTemplateId, qtn.formatUuid) of
      (Just tmlId, Just formatUuid) -> do
        tml <- getDocumentTemplateByUuidAndPackageId tmlId (Just qtn.packageId)
        createDocumentPreview tml qtn formatUuid
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP

createDocumentPreviewForDocTmlDraft :: String -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreviewForDocTmlDraft tmlId =
  runInTransaction $ do
    draftData <- findDraftDataById tmlId
    case (draftData.questionnaireUuid, draftData.formatUuid) of
      (Just qtnUuid, Just formatUuid) -> do
        draft <- findDraftById tmlId
        qtn <- findQuestionnaireByUuid qtnUuid
        checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
        createDocumentPreview draft qtn formatUuid
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__QUESTIONNAIRE_OR_FORMAT_NOT_SET_UP

createDocumentPreview :: DocumentTemplate -> Questionnaire -> U.UUID -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreview tml qtn formatUuid = do
  docs <- findDocumentsFiltered [("questionnaire_uuid", U.toString qtn.uuid), ("durability", "TemporallyDocumentDurability")]
  qtnCtn <- compileQuestionnaire qtn
  appConfig <- getAppConfig
  mCurrentUser <- asks currentUser
  let repliesHash = computeHash qtn qtnCtn appConfig mCurrentUser
  logDebugI _CMP_SERVICE ("Replies hash: " ++ show repliesHash)
  let matchingDocs = filter (\d -> d.questionnaireRepliesHash == repliesHash) docs
  case filter (filterAlreadyDoneDocument tml.tId formatUuid) matchingDocs of
    (doc : _) -> do
      logInfoI _CMP_SERVICE "Retrieving from cache"
      if doc.state == DoneDocumentState
        then do
          let expirationInSeconds = 60
          link <- presigneGetDocumentUrl doc.uuid expirationInSeconds
          return (doc, TemporaryFileDTO link (fromMaybe "text/plain" doc.contentType))
        else return (doc, TemporaryFileMapper.emptyFileDTO)
    [] ->
      case filter (\d -> d.state == QueuedDocumentState || d.state == InProgressDocumentState) matchingDocs of
        (doc : _) -> do
          logInfoI _CMP_SERVICE "Waiting to generation"
          return (doc, TemporaryFileMapper.emptyFileDTO)
        _ -> do
          logInfoI _CMP_SERVICE "Generating new preview"
          validateMetamodelVersion tml
          dUuid <- liftIO generateUuid
          now <- liftIO getCurrentTime
          let doc = fromTemporallyCreateDTO dUuid qtn tml.tId formatUuid repliesHash mCurrentUser appConfig.uuid now
          insertDocument doc
          publishToPersistentCommandQueue doc
          return (doc, TemporaryFileMapper.emptyFileDTO)

publishToPersistentCommandQueue :: Document -> AppContextM ()
publishToPersistentCommandQueue doc = do
  docContext <- createDocumentContext doc
  pUuid <- liftIO generateUuid
  let command = toDocPersistentCommand pUuid docContext doc
  insertPersistentCommand command
  return ()
