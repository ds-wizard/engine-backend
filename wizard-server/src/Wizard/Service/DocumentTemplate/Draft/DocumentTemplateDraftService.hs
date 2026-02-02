module Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.KnowledgeModel.Localization.Messages.Public
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation hiding (validateChangeDto)
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftValidation
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService
import Wizard.Service.Tenant.Limit.LimitService

getDraftsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateDraftList)
getDraftsPage mQuery pageable sort = do
  checkPermission _DOC_TML_WRITE_PERM
  findDraftsPage mQuery pageable sort

createDraft :: DocumentTemplateDraftCreateDTO -> AppContextM DocumentTemplateSimple
createDraft reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkDocumentTemplateDraftLimit
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tcOrganization <- findTenantConfigOrganization
    case reqDto.basedOn of
      Just dtUuid -> do
        tml <- findDocumentTemplateByUuid dtUuid
        formats <- findDocumentTemplateFormats dtUuid
        when
          tml.nonEditable
          (throwError . UserError $ _ERROR_SERVICE_DOC_TML__NON_EDITABLE_DOC_TML)
        let (draft, draftFormats) = fromCreateDTO reqDto uuid tml formats tcOrganization.organizationId now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        traverse_ insertDocumentTemplateFormat draftFormats
        assets <- findAssetsByDocumentTemplateUuid dtUuid
        traverse_ (duplicateAsset draft.uuid) assets
        files <- findFilesByDocumentTemplateUuid dtUuid
        traverse_ (duplicateFile draft.uuid) files
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return $ toSimple draft
      Nothing -> do
        let draft = fromCreateDTO' reqDto uuid tcOrganization.organizationId tcOrganization.tenantUuid now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return $ toSimple draft

getDraft :: U.UUID -> AppContextM DocumentTemplateDraftDetail
getDraft dtUuid = do
  checkPermission _DOC_TML_WRITE_PERM
  draft <- findDraftByUuid dtUuid
  formats <- findDocumentTemplateFormats draft.uuid
  draftData <- findDraftDataByUuid dtUuid
  mProjectSuggestion <-
    case draftData.projectUuid of
      Just projectUuid -> findProjectSuggestionByUuid' projectUuid
      Nothing -> return Nothing
  mKmEditorSuggestion <-
    case draftData.knowledgeModelEditorUuid of
      Just knowledgeModelEditorUuid -> findKnowledgeModelEditorSuggestionByUuid' knowledgeModelEditorUuid
      Nothing -> return Nothing
  return $ toDraftDetail draft formats draftData mProjectSuggestion mKmEditorSuggestion

modifyDraft :: U.UUID -> DocumentTemplateDraftChangeDTO -> AppContextM DocumentTemplateDraftDetail
modifyDraft dtUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    -- Update draft
    now <- liftIO getCurrentTime
    draft <- findDraftByUuid dtUuid
    validateChangeDto reqDto draft
    let draftUpdated = fromChangeDTO reqDto draft now
    updateDocumentTemplateById draftUpdated
    -- Update formats
    let formatsUpdated = fmap (fromFormatDTO dtUuid draft.tenantUuid draft.createdAt now) reqDto.formats
    traverse_ insertOrUpdateDocumentTemplateFormat formatsUpdated
    deleteDocumentTemplateFormatsExcept dtUuid (fmap (.uuid) formatsUpdated)
    -- Delete temporary documents for the template
    deleteTemporalDocumentsByDocumentTemplateUuid dtUuid
    when (reqDto.phase == ReleasedDocumentTemplatePhase) (void $ deleteDraftDataByDocumentTemplateUuid dtUuid)
    return $ toDraftDetail' draftUpdated formatsUpdated

modifyDraftData :: U.UUID -> DocumentTemplateDraftDataChangeDTO -> AppContextM DocumentTemplateDraftDataDTO
modifyDraftData dtUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    draftData <- findDraftDataByUuid dtUuid
    let updatedDraftData = fromDraftDataChangeDTO draftData reqDto
    updateDraftDataById updatedDraftData
    mProjectSuggestion <-
      case updatedDraftData.projectUuid of
        Just projectUuid -> findProjectSuggestionByUuid' projectUuid
        Nothing -> return Nothing
    mKmEditorSuggestion <-
      case draftData.knowledgeModelEditorUuid of
        Just knowledgeModelEditorUuid -> findKnowledgeModelEditorSuggestionByUuid' knowledgeModelEditorUuid
        Nothing -> return Nothing
    return $ toDraftDataDTO updatedDraftData mProjectSuggestion mKmEditorSuggestion

deleteDraft :: U.UUID -> AppContextM ()
deleteDraft uuid =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    draft <- findDraftByUuid uuid
    void $ deleteDraftByUuid uuid
