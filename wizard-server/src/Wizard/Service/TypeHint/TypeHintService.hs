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
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Integration.Http.TypeHint.Runner
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Config.Integration.IntegrationConfigService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

getLegacyTypeHints :: TypeHintLegacyRequestDTO -> AppContextM [TypeHintLegacyIDTO]
getLegacyTypeHints reqDto =
  runInTransaction $ do
    km <- compileKnowledgeModel reqDto.events reqDto.packageId []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiLegacyIntegration' integration -> do
        fileIntConfig <- getFileIntegrationConfig integration.iId
        appIntConfig <- getTenantIntegrationConfig integration.iId
        let configs = M.union question.props . M.union appIntConfig $ fileIntConfig
        let variables = M.insert "q" reqDto.q configs
        let urlVariables = M.insert "q" (encode reqDto.q) configs
        eiDtos <- retrieveLegacyTypeHints integration urlVariables variables
        case eiDtos of
          Right iDtos -> return iDtos
          Left error -> do
            logWarnI _CMP_SERVICE error
            throwError . UserError $ _ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION

getTypeHints :: TypeHintRequestDTO -> AppContextM [TypeHintIDTO]
getTypeHints (BranchIntegrationTypeHintRequest' reqDto) =
  runInTransaction $ do
    checkPermission _KM_PERM
    branch <- findBranchByUuid reqDto.branchUuid
    branchData <- findBranchDataById reqDto.branchUuid
    km <- compileKnowledgeModel branchData.events branch.previousPackageId []
    integration' <- getIntegration km reqDto.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration integration.testVariables integration.testQ
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
getTypeHints (BranchQuestionTypeHintRequest' reqDto) =
  runInTransaction $ do
    checkPermission _KM_PERM
    branch <- findBranchByUuid reqDto.branchUuid
    branchData <- findBranchDataById reqDto.branchUuid
    km <- compileKnowledgeModel branchData.events branch.previousPackageId []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration question.props reqDto.q
      _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
getTypeHints (QuestionnaireTypeHintRequest' reqDto) =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid reqDto.questionnaireUuid
    checkEditPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    km <- compileKnowledgeModel [] (Just qtn.packageId) []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> runApiIntegrationTypeHints integration question.props reqDto.q
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
    branch <- findBranchByUuid reqDto.branchUuid
    branchData <- findBranchDataById reqDto.branchUuid
    km <- compileKnowledgeModel branchData.events branch.previousPackageId []
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
