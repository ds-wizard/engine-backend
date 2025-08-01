module Wizard.Service.TypeHint.TypeHintService where

import Control.Monad.Except (throwError)
import Data.Map.Strict as M
import Network.URI.Encode (encode)

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModelSecret.KnowledgeModelSecretDAO
import Wizard.Integration.Http.TypeHint.Runner
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.Integration.IntegrationConfigService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

getLegacyTypeHints :: TypeHintRequestDTO -> AppContextM [TypeHintLegacyIDTO]
getLegacyTypeHints reqDto =
  runInTransaction $ do
    km <- compileKnowledgeModel reqDto.events reqDto.packageId []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiLegacyIntegration' integration -> do
        fileIntConfig <- getFileIntegrationConfig integration.iId
        appIntConfig <- getTenantIntegrationConfig integration.iId
        let kmQuestionConfig = question.props
        let encodedUserRequest = M.singleton "q" (encode reqDto.q)
        let userRequest = M.singleton "q" reqDto.q
        let encodedVariables = M.union encodedUserRequest . M.union kmQuestionConfig . M.union appIntConfig $ fileIntConfig
        let variables = M.union userRequest . M.union kmQuestionConfig . M.union appIntConfig $ fileIntConfig
        eiDtos <- retrieveLegacyTypeHints integration encodedVariables variables
        case eiDtos of
          Right iDtos -> return iDtos
          Left error -> do
            logWarnI _CMP_SERVICE error
            throwError . UserError $ _ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

getTypeHints :: TypeHintRequestDTO -> AppContextM [TypeHintIDTO]
getTypeHints reqDto =
  runInTransaction $ do
    km <- compileKnowledgeModel reqDto.events reqDto.packageId []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    secrets <- findKnowledgeModelSecrets
    case integration' of
      ApiIntegration' integration -> do
        -- TODO: add KM secrets
        let questionVariables = question.props
        let userQuery = M.singleton "q" reqDto.q
        let variables = M.union userQuery questionVariables
        eiDtos <- retrieveTypeHints integration variables
        case eiDtos of
          Right iDtos -> return iDtos
          Left error -> do
            logWarnI _CMP_SERVICE error
            throwError . UserError $ _ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

testTypeHints :: TypeHintTestRequestDTO -> AppContextM TypeHintResponse
testTypeHints reqDto =
  runInTransaction $ do
    checkPermission _KM_PERM
    branch <- findBranchByUuid reqDto.branchUuid
    branchData <- findBranchDataById reqDto.branchUuid
    km <- compileKnowledgeModel branchData.events branch.previousPackageId []
    integration' <- getIntegration km reqDto.integrationUuid
    secrets <- findKnowledgeModelSecrets
    case integration' of
      ApiIntegration' integration -> do
        -- TODO: add KM secrets
        let questionVariables = reqDto.variables
        let userQuery = M.singleton "q" reqDto.q
        let variables = M.union userQuery questionVariables
        testRetrieveTypeHints integration variables
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

-- helpers
getQuestion km questionUuid =
  case M.lookup questionUuid (getQuestionsM km) of
    Just (IntegrationQuestion' question) -> return question
    Just _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__QUESTION_ABSENCE
getIntegration km integrationUuid =
  case M.lookup integrationUuid (getIntegrationsM km) of
    Just integration -> return integration
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__INTEGRATION_ABSENCE
