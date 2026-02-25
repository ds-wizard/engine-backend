module Wizard.Service.TypeHint.TypeHintService where

import Control.Monad.Except (throwError)
import Data.Map.Strict as M

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Integration.Http.TypeHint.Runner
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret
import Wizard.Model.Project.Project
import Wizard.Service.Config.Integration.IntegrationConfigService
import Wizard.Service.KnowledgeModel.Editor.EditorMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Project.ProjectAcl

getTypeHints :: TypeHintRequestDTO -> AppContextM [TypeHintIDTO]
getTypeHints (KnowledgeModelEditorIntegrationTypeHintRequest' reqDto) =
  runInTransaction $ do
    checkPermission _KM_PERM
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.knowledgeModelEditorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid reqDto.knowledgeModelEditorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    km <- compileKnowledgeModel kmEvents kmEditor.previousPackageUuid []
    integration' <- getIntegration km reqDto.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration integration.testVariables integration.testQ
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
getTypeHints (KnowledgeModelEditorQuestionTypeHintRequest' reqDto) =
  runInTransaction $ do
    checkPermission _KM_PERM
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.knowledgeModelEditorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid reqDto.knowledgeModelEditorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    km <- compileKnowledgeModel kmEvents kmEditor.previousPackageUuid []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration question.variables reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
getTypeHints (ProjectTypeHintRequest' reqDto) =
  runInTransaction $ do
    project <- findProjectByUuid reqDto.projectUuid
    checkEditPermissionToProject project.visibility project.sharing project.permissions
    km <- compileKnowledgeModel [] (Just project.knowledgeModelPackageUuid) []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration question.variables reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

runApiIntegrationTypeHints :: ApiIntegration -> M.Map String String -> String -> AppContextM [TypeHintIDTO]
runApiIntegrationTypeHints integration variables q =
  runInTransaction $ do
    secrets <- prepareSecrets
    eiDtos <- retrieveTypeHints integration variables secrets q
    case eiDtos of
      Right iDtos -> return iDtos
      Left error -> do
        logWarnI _CMP_SERVICE error
        throwError . UserError $ _ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR

testTypeHints :: TypeHintTestRequestDTO -> AppContextM TypeHintExchange
testTypeHints reqDto =
  runInTransaction $ do
    checkPermission _KM_PERM
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.knowledgeModelEditorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid reqDto.knowledgeModelEditorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    km <- compileKnowledgeModel kmEvents kmEditor.previousPackageUuid []
    integration' <- getIntegration km reqDto.integrationUuid
    case integration' of
      ApiIntegration' integration -> do
        secrets <- prepareSecrets
        testRetrieveTypeHints integration reqDto.variables secrets reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

-- --------------------------------
-- PRIVATE
-- --------------------------------
getQuestion km questionUuid =
  case M.lookup questionUuid (getQuestionsM km) of
    Just (IntegrationQuestion' question) -> return question
    Just _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__QUESTION_ABSENCE

getIntegration km integrationUuid =
  case M.lookup integrationUuid (getIntegrationsM km) of
    Just integration -> return integration
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__INTEGRATION_ABSENCE

prepareSecrets :: AppContextM (M.Map String String)
prepareSecrets = do
  kmSecrets <- fmap (M.fromList . fmap (\s -> (s.name, s.value))) findKnowledgeModelSecrets
  fileSecrets <- getFileIntegrationConfig "secrets"
  return $ M.union kmSecrets fileSecrets
