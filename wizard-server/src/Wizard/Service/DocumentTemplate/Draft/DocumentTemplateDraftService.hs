module Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time

import Shared.Common.Api.Resource.Common.EntityCreatedWithIdDTO
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
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

createDraft :: DocumentTemplateDraftCreateDTO -> AppContextM EntityCreatedWithIdDTO
createDraft reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkDocumentTemplateDraftLimit
    now <- liftIO getCurrentTime
    tcOrganization <- findTenantConfigOrganization
    case reqDto.basedOn of
      Just tmlId -> do
        tml <- findDocumentTemplateById tmlId
        formats <- findDocumentTemplateFormats tmlId
        when
          tml.nonEditable
          (throwError . UserError $ _ERROR_SERVICE_DOC_TML__NON_EDITABLE_DOC_TML)
        let (draft, draftFormats) = fromCreateDTO reqDto tml formats tcOrganization.organizationId now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        traverse_ insertDocumentTemplateFormat draftFormats
        assets <- findAssetsByDocumentTemplateId tmlId
        traverse_ (duplicateAsset draft.tId) assets
        files <- findFilesByDocumentTemplateId tmlId
        traverse_ (duplicateFile draft.tId) files
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return $ EntityCreatedWithIdDTO draft.tId
      Nothing -> do
        let draft = fromCreateDTO' reqDto tcOrganization.organizationId tcOrganization.tenantUuid now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return $ EntityCreatedWithIdDTO draft.tId

getDraft :: String -> AppContextM DocumentTemplateDraftDetail
getDraft tmlId = do
  checkPermission _DOC_TML_WRITE_PERM
  draft <- findDraftById tmlId
  formats <- findDocumentTemplateFormats draft.tId
  draftData <- findDraftDataById tmlId
  mProjectSuggestion <-
    case draftData.projectUuid of
      Just projectUuid -> findProjectSuggestionByUuid' projectUuid
      Nothing -> return Nothing
  mKmEditorSuggestion <-
    case draftData.knowledgeModelEditorUuid of
      Just knowledgeModelEditorUuid -> findKnowledgeModelEditorSuggestionByUuid' knowledgeModelEditorUuid
      Nothing -> return Nothing
  return $ toDraftDetail draft formats draftData mProjectSuggestion mKmEditorSuggestion

modifyDraft :: String -> DocumentTemplateDraftChangeDTO -> AppContextM DocumentTemplateDraftDetail
modifyDraft tmlId reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    -- Update draft
    now <- liftIO getCurrentTime
    draft <- findDraftById tmlId
    validateChangeDto reqDto draft
    let draftUpdated = fromChangeDTO reqDto draft now
    updateDocumentTemplateById draftUpdated
    -- Update formats
    let formatsUpdated = fmap (fromFormatDTO tmlId draft.tenantUuid draft.createdAt now) reqDto.formats
    traverse_ insertOrUpdateDocumentTemplateFormat formatsUpdated
    deleteDocumentTemplateFormatsExcept tmlId (fmap (.uuid) formatsUpdated)
    -- Delete temporary documents for the template
    deleteTemporalDocumentsByDocumentTemplateId tmlId
    if reqDto.phase == ReleasedDocumentTemplatePhase
      then do
        deleteDraftDataByDocumentTemplateId tmlId
        return $ toDraftDetail' draftUpdated formatsUpdated
      else do
        if reqDto.templateId /= draft.templateId || reqDto.version /= draft.version
          then do
            let createDto =
                  DocumentTemplateDraftCreateDTO
                    { name = reqDto.name
                    , templateId = reqDto.templateId
                    , version = reqDto.version
                    , basedOn = Just tmlId
                    }
            newDraft <- createDraft createDto
            deleteDraft tmlId
            getDraft newDraft.aId
          else getDraft tmlId

modifyDraftData :: String -> DocumentTemplateDraftDataChangeDTO -> AppContextM DocumentTemplateDraftDataDTO
modifyDraftData tmlId reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    draftData <- findDraftDataById tmlId
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

deleteDraft :: String -> AppContextM ()
deleteDraft tmlId =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    draft <- findDraftById tmlId
    void $ deleteDraftByDocumentTemplateId tmlId
