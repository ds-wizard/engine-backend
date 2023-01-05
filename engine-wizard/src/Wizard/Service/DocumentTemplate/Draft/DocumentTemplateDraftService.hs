module Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftService where

import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Document.DocumentCleanService
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation hiding (validateChangeDto)
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftValidation
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService
import Wizard.Service.Limit.AppLimitService

getDraftsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateDraftList)
getDraftsPage mQuery pageable sort = do
  checkPermission _DOC_TML_WRITE_PERM
  findDraftsPage mQuery pageable sort

createDraft :: DocumentTemplateDraftCreateDTO -> AppContextM DocumentTemplate
createDraft reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkDocumentTemplateDraftLimit
    now <- liftIO getCurrentTime
    appConfig <- getAppConfig
    case reqDto.basedOn of
      Just tmlId -> do
        tml <- findDocumentTemplateById tmlId
        let draft = fromCreateDTO reqDto tml appConfig.organization.organizationId now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        assets <- findAssetsByDocumentTemplateId tmlId
        traverse_ (duplicateAsset draft.tId) assets
        files <- findFilesByDocumentTemplateId tmlId
        traverse_ (duplicateFile draft.tId) files
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return draft
      Nothing -> do
        let draft = fromCreateDTO' reqDto appConfig.organization.organizationId appConfig.uuid now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return draft

getDraft :: String -> AppContextM DocumentTemplateDraftDetail
getDraft tmlId = do
  checkPermission _DOC_TML_WRITE_PERM
  draft <- findDraftById tmlId
  draftData <- findDraftDataById tmlId
  mQtnSuggestion <-
    case draftData.questionnaireUuid of
      Just qtnUuid -> findQuestionnaireSuggestionByUuid' qtnUuid
      Nothing -> return Nothing
  return $ toDraftDetail draft draftData mQtnSuggestion

modifyDraft :: String -> DocumentTemplateDraftChangeDTO -> AppContextM DocumentTemplateDraftDetail
modifyDraft tmlId reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    draft <- findDraftById tmlId
    validateChangeDto reqDto draft
    let draftUpdated = fromChangeDTO reqDto draft
    updateDocumentTemplateById draftUpdated
    deleteTemporalDocumentsByDocumentTemplateId tmlId
    if reqDto.phase == ReleasedDocumentTemplatePhase
      then do
        deleteDraftDataByDocumentTemplateId tmlId
        return . toDraftDetail' $ draftUpdated
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
            getDraft newDraft.tId
          else getDraft tmlId

modifyDraftData :: String -> DocumentTemplateDraftDataChangeDTO -> AppContextM DocumentTemplateDraftDataDTO
modifyDraftData tmlId reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    draftData <- findDraftDataById tmlId
    let updatedDraftData = fromDraftDataChangeDTO draftData reqDto
    updateDraftDataById updatedDraftData
    mQtnSuggestion <-
      case updatedDraftData.questionnaireUuid of
        Just qtnUuid -> findQuestionnaireSuggestionByUuid' qtnUuid
        Nothing -> return Nothing
    return $ toDraftDataDTO updatedDraftData mQtnSuggestion

deleteDraft :: String -> AppContextM ()
deleteDraft tmlId =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    draft <- findDraftById tmlId
    assets <- findAssetsByDocumentTemplateId tmlId
    cleanTemporallyDocumentsForTemplate tmlId
    validateDocumentTemplateDeletation tmlId
    deleteDraftByDocumentTemplateId tmlId
    let assetUuids = fmap (.uuid) assets
    traverse_ (removeAsset tmlId) assetUuids
