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
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.Project.Event.ProjectEventListLenses ()
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Project.Version.ProjectVersion
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
import Wizard.Service.Project.Compiler.ProjectCompilerService
import Wizard.Service.Project.ProjectAcl
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getDocumentsPageDto :: Maybe U.UUID -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsPageDto mProjectUuid mDocumentTemplateId mQuery pageable sort = do
  checkPermission _DOC_PERM
  docPage <- findDocumentsPage mProjectUuid Nothing mDocumentTemplateId mQuery pageable sort
  traverse enhanceDocument docPage

getDocumentsForProject :: U.UUID -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentDTO)
getDocumentsForProject projectUuid mQuery pageable sort = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToDoc' project
  docPage <- findDocumentsPage (Just projectUuid) (Just project.name) Nothing mQuery pageable sort
  traverse enhanceDocument docPage

createDocument :: DocumentCreateDTO -> AppContextM DocumentDTO
createDocument reqDto =
  runInTransaction $ do
    checkEditPermissionToDoc (Just reqDto.projectUuid)
    checkDocumentLimit
    checkStorageSize 0
    project <- findProjectByUuid reqDto.projectUuid
    tml <- getDocumentTemplateByUuidAndPackageId reqDto.documentTemplateId (Just project.knowledgeModelPackageId)
    format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid reqDto.documentTemplateId reqDto.formatUuid
    validateMetamodelVersion tml
    uuid <- liftIO generateUuid
    mCurrentUser <- asks currentUser
    now <- liftIO getCurrentTime
    projectEvents <- findProjectEventListsByProjectUuid project.uuid
    let filteredProjectEvents =
          case reqDto.projectEventUuid of
            Just eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) projectEvents
            Nothing -> projectEvents
    let projectContent = compileProjectEvents filteredProjectEvents
    tcOrganization <- findTenantConfigOrganization
    projectVersions <- findProjectVersionsByProjectUuid project.uuid
    let docContextHash = computeHash [] project projectVersions projectContent.phaseUuid projectContent.replies tcOrganization mCurrentUser
    let doc = fromCreateDTO reqDto uuid docContextHash filteredProjectEvents mCurrentUser project.tenantUuid now
    insertDocument doc
    pkg <- getPackageById project.knowledgeModelPackageId
    publishToPersistentCommandQueue doc pkg [] project Nothing
    return $ toDTOWithDocTemplate doc project Nothing [] tml format

deleteDocument :: U.UUID -> AppContextM ()
deleteDocument docUuid =
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkEditPermissionToDoc doc.projectUuid
    void $ deleteDocumentByUuid docUuid

downloadDocument :: U.UUID -> AppContextM TemporaryFileDTO
downloadDocument docUuid = do
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkViewPermissionToDoc doc.projectUuid
    content <- retrieveDocumentContent docUuid
    let fileName = fromMaybe "export" doc.fileName
    let contentType = fromMaybe "text/plain" doc.contentType
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile fileName "application/octet-stream" mCurrentUserUuid (BSL.fromStrict content)
    return $ TemporaryFileMapper.toDTO url contentType

createDocumentPreviewForProject :: U.UUID -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreviewForProject projectUuid =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkViewPermissionToProject project.visibility project.sharing project.permissions
    case (project.documentTemplateId, project.formatUuid) of
      (Just tmlId, Just formatUuid) -> do
        tml <- getDocumentTemplateByUuidAndPackageId tmlId (Just project.knowledgeModelPackageId)
        pkg <- getPackageById project.knowledgeModelPackageId
        projectEvents <- findProjectEventListsByProjectUuid projectUuid
        let projectEventUuid = fmap getUuid (lastSafe projectEvents)
        let projectContent = compileProjectEvents projectEvents
        projectVersions <- findProjectVersionsByProjectUuid project.uuid
        createDocumentPreview tml pkg [] project projectVersions projectEventUuid projectContent.phaseUuid projectContent.replies formatUuid False
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP

createDocumentPreviewForDocTmlDraft :: String -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreviewForDocTmlDraft tmlId =
  runInTransaction $ do
    draftData <- findDraftDataById tmlId
    case (draftData.projectUuid, draftData.knowledgeModelEditorUuid, draftData.formatUuid) of
      (Just projectUuid, _, Just formatUuid) -> do
        draft <- findDraftById tmlId
        project <- findProjectByUuid projectUuid
        pkg <- getPackageById project.knowledgeModelPackageId
        checkViewPermissionToProject project.visibility project.sharing project.permissions
        projectEvents <- findProjectEventListsByProjectUuid project.uuid
        let projectEventUuid = fmap getUuid (lastSafe projectEvents)
        let projectContent = compileProjectEvents projectEvents
        projectVersions <- findProjectVersionsByProjectUuid project.uuid
        createDocumentPreview draft pkg [] project projectVersions projectEventUuid projectContent.phaseUuid projectContent.replies formatUuid False
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
        let project = toTemporaryProject editor pkg mCurrentUser
        let projectEventUuid = Nothing
        createDocumentPreview draft pkg kmEvents project [] projectEventUuid Nothing replies formatUuid True
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__PROJECT_OR_FORMAT_NOT_SET_UP

createDocumentPreview :: DocumentTemplate -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Project -> [ProjectVersion] -> Maybe U.UUID -> Maybe U.UUID -> M.Map String Reply -> U.UUID -> Bool -> AppContextM (Document, TemporaryFileDTO)
createDocumentPreview tml pkg kmEditorEvents project projectVersions projectEventUuid phaseUuid replies formatUuid fromKnowledgeModelEditor = do
  tcOrganization <- findTenantConfigOrganization
  mCurrentUser <- asks currentUser
  let repliesHash = computeHash kmEditorEvents project projectVersions phaseUuid replies tcOrganization mCurrentUser
  logDebugI _CMP_SERVICE ("Replies hash: " ++ show repliesHash)
  docs <-
    if fromKnowledgeModelEditor
      then findDocumentsForCurrentTenantFiltered [("project_replies_hash", show repliesHash), ("durability", "TemporallyDocumentDurability")]
      else findDocumentsForCurrentTenantFiltered [("project_uuid", U.toString project.uuid), ("project_replies_hash", show repliesHash), ("durability", "TemporallyDocumentDurability")]
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
          let doc = fromTemporallyCreateDTO dUuid project projectEventUuid tml.tId formatUuid repliesHash mCurrentUser tcOrganization.tenantUuid now fromKnowledgeModelEditor
          insertDocument doc
          let mReplies = if fromKnowledgeModelEditor then Just replies else Nothing
          publishToPersistentCommandQueue doc pkg kmEditorEvents project mReplies
          return (doc, TemporaryFileMapper.emptyFileDTO)

publishToPersistentCommandQueue :: Document -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Project -> Maybe (M.Map String Reply) -> AppContextM ()
publishToPersistentCommandQueue doc pkg kmEditorEvents project mReplies = do
  docContext <- createDocumentContext doc pkg kmEditorEvents project mReplies
  pUuid <- liftIO generateUuid
  let command = toDocPersistentCommand pUuid docContext doc
  insertPersistentCommand command
  return ()
