module Wizard.Service.Document.DocumentService where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import qualified Data.Map as M
import Shared.Common.Constant.Component
import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.Common.Util.List
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Document.Context.DocumentContextService
import Wizard.Service.Document.DocumentAcl
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Document.DocumentUtil
import Wizard.Service.DocumentTemplate.DocumentTemplateService
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import qualified Wizard.Service.KnowledgeModel.Editor.EditorMapper as EditorMapper
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getDocumentsPageDto :: Maybe U.UUID -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsPageDto mQuestionnaireUuid mDocumentTemplateId mQuery pageable sort = do
  checkPermission _DOC_PERM
  docPage <- findDocumentsPage mQuestionnaireUuid Nothing mDocumentTemplateId mQuery pageable sort
  traverse enhanceDocument docPage

getDocumentsForQtn :: U.UUID -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsForQtn qtnUuid mQuery pageable sort = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkViewPermissionToDoc' qtn
  docPage <- findDocumentsPage (Just qtnUuid) (Just qtn.name) Nothing mQuery pageable sort
  traverse enhanceDocument docPage

createDocument :: DocumentCreateDTO -> AppContextM DocumentDTO
createDocument reqDto =
  runInTransaction $ do
    checkEditPermissionToDoc (Just reqDto.questionnaireUuid)
    checkDocumentLimit
    checkStorageSize 0
    qtn <- findQuestionnaireByUuid reqDto.questionnaireUuid
    tml <- getDocumentTemplateByUuidAndPackageId reqDto.documentTemplateId (Just qtn.knowledgeModelPackageId)
    format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid reqDto.documentTemplateId reqDto.formatUuid
    validateMetamodelVersion tml
    uuid <- liftIO generateUuid
    mCurrentUser <- asks currentUser
    now <- liftIO getCurrentTime
    qtnEvents <- findQuestionnaireEventsByQuestionnaireUuid qtn.uuid
    let filteredQtnEvents =
          case reqDto.questionnaireEventUuid of
            Just eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) qtnEvents
            Nothing -> qtnEvents
    qtnCtn <- compileQuestionnairePreview filteredQtnEvents
    tcOrganization <- findTenantConfigOrganization
    qtnVersions <- findQuestionnaireVersionsByQuestionnaireUuid qtn.uuid
    let docContextHash = computeHash [] qtn qtnVersions qtnCtn.phaseUuid qtnCtn.replies tcOrganization mCurrentUser
    let doc = fromCreateDTO reqDto uuid docContextHash filteredQtnEvents mCurrentUser qtn.tenantUuid now
    insertDocument doc
    pkg <- getPackageById qtn.knowledgeModelPackageId
    publishToPersistentCommandQueue doc pkg [] qtn Nothing
    return $ toDTOWithDocTemplate doc qtn Nothing [] tml format

deleteDocument :: U.UUID -> AppContextM ()
deleteDocument docUuid =
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkEditPermissionToDoc doc.questionnaireUuid
    void $ deleteDocumentByUuid docUuid

downloadDocument :: U.UUID -> AppContextM TemporaryFileDTO
downloadDocument docUuid = do
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkViewPermissionToDoc doc.questionnaireUuid
    content <- retrieveDocumentContent docUuid
    let fileName = fromMaybe "export" doc.fileName
    let contentType = fromMaybe "text/plain" doc.contentType
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile fileName "application/octet-stream" mCurrentUserUuid (BSL.fromStrict content)
    return $ TemporaryFileMapper.toDTO url contentType

createDocumentPreviewForQtn :: U.UUID -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreviewForQtn qtnUuid =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    case (qtn.documentTemplateId, qtn.formatUuid) of
      (Just tmlId, Just formatUuid) -> do
        tml <- getDocumentTemplateByUuidAndPackageId tmlId (Just qtn.knowledgeModelPackageId)
        pkg <- getPackageById qtn.knowledgeModelPackageId
        qtnEvents <- findQuestionnaireEventsByQuestionnaireUuid qtnUuid
        let questionnaireEventUuid = fmap getUuid (lastSafe qtnEvents)
        qtnCtn <- compileQuestionnaire qtnEvents
        qtnVersions <- findQuestionnaireVersionsByQuestionnaireUuid qtn.uuid
        createDocumentPreview tml pkg [] qtn qtnVersions questionnaireEventUuid qtnCtn.phaseUuid qtnCtn.replies formatUuid False
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP

createDocumentPreviewForDocTmlDraft :: String -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreviewForDocTmlDraft tmlId =
  runInTransaction $ do
    draftData <- findDraftDataById tmlId
    case (draftData.questionnaireUuid, draftData.knowledgeModelEditorUuid, draftData.formatUuid) of
      (Just qtnUuid, _, Just formatUuid) -> do
        draft <- findDraftById tmlId
        qtn <- findQuestionnaireByUuid qtnUuid
        pkg <- getPackageById qtn.knowledgeModelPackageId
        checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
        qtnEvents <- findQuestionnaireEventsByQuestionnaireUuid qtn.uuid
        let questionnaireEventUuid = fmap getUuid (lastSafe qtnEvents)
        qtnCtn <- compileQuestionnaire qtnEvents
        qtnVersions <- findQuestionnaireVersionsByQuestionnaireUuid qtn.uuid
        createDocumentPreview draft pkg [] qtn qtnVersions questionnaireEventUuid qtnCtn.phaseUuid qtnCtn.replies formatUuid False
      (_, Just kmEditorUuid, Just formatUuid) -> do
        draft <- findDraftById tmlId
        let pkg = toTemporaryPackage draft.tenantUuid draft.createdAt
        editor <- findKnowledgeModelEditorByUuid kmEditorUuid
        kmEditorEvents <- findKnowledgeModelEventsByEditorUuid kmEditorUuid
        let kmEvents = fmap EditorMapper.toKnowledgeModelEvent kmEditorEvents
        kmEditorReplies <- findKnowledgeModelRepliesByEditorUuid kmEditorUuid
        let replies = EditorMapper.toReplies kmEditorReplies
        checkPermission _KM_PERM
        mCurrentUser <- asks currentUser
        let qtn = toTemporaryQuestionnaire editor pkg mCurrentUser
        let questionnaireEventUuid = Nothing
        createDocumentPreview draft pkg kmEvents qtn [] questionnaireEventUuid Nothing replies formatUuid True
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__QUESTIONNAIRE_OR_FORMAT_NOT_SET_UP

createDocumentPreview :: DocumentTemplate -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Questionnaire -> [QuestionnaireVersion] -> Maybe U.UUID -> Maybe U.UUID -> M.Map String Reply -> U.UUID -> Bool -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreview tml pkg kmEditorEvents qtn qtnVersions questionnaireEventUuid phaseUuid replies formatUuid fromKnowledgeModelEditor = do
  tcOrganization <- findTenantConfigOrganization
  mCurrentUser <- asks currentUser
  let repliesHash = computeHash kmEditorEvents qtn qtnVersions phaseUuid replies tcOrganization mCurrentUser
  logDebugI _CMP_SERVICE ("Replies hash: " ++ show repliesHash)
  docs <-
    if fromKnowledgeModelEditor
      then findDocumentsForCurrentTenantFiltered [("questionnaire_replies_hash", show repliesHash), ("durability", "TemporallyDocumentDurability")]
      else findDocumentsForCurrentTenantFiltered [("questionnaire_uuid", U.toString qtn.uuid), ("questionnaire_replies_hash", show repliesHash), ("durability", "TemporallyDocumentDurability")]
  case filter (filterAlreadyDoneDocument tml.tId formatUuid) docs of
    (doc : _) -> do
      logInfoI _CMP_SERVICE "Retrieving from cache"
      if doc.state == DoneDocumentState
        then do
          let expirationInSeconds = 60
          link <- presignGetDocumentUrl doc.uuid expirationInSeconds
          return (doc, TemporaryFileDTO link (fromMaybe "text/plain" doc.contentType))
        else return (doc, TemporaryFileMapper.emptyFileDTO)
    [] ->
      case filter (\d -> d.state == QueuedDocumentState || d.state == InProgressDocumentState) docs of
        (doc : _) -> do
          logInfoI _CMP_SERVICE "Waiting to generation"
          return (doc, TemporaryFileMapper.emptyFileDTO)
        _ -> do
          logInfoI _CMP_SERVICE "Generating new preview"
          validateMetamodelVersion tml
          dUuid <- liftIO generateUuid
          now <- liftIO getCurrentTime
          let doc = fromTemporallyCreateDTO dUuid qtn questionnaireEventUuid tml.tId formatUuid repliesHash mCurrentUser tcOrganization.tenantUuid now fromKnowledgeModelEditor
          insertDocument doc
          let mReplies = if fromKnowledgeModelEditor then Just replies else Nothing
          publishToPersistentCommandQueue doc pkg kmEditorEvents qtn mReplies
          return (doc, TemporaryFileMapper.emptyFileDTO)

publishToPersistentCommandQueue :: Document -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Questionnaire -> Maybe (M.Map String Reply) -> AppContextM ()
publishToPersistentCommandQueue doc pkg kmEditorEvents qtn mReplies = do
  docContext <- createDocumentContext doc pkg kmEditorEvents qtn mReplies
  pUuid <- liftIO generateUuid
  let command = toDocPersistentCommand pUuid docContext doc
  insertPersistentCommand command
  return ()
