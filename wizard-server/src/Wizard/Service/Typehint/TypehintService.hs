module Wizard.Service.Typehint.TypehintService where

import Control.Monad.Except (throwError)
import Data.Map.Strict as M
import Network.URI.Encode (encode)

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Typehint.Runner
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.Integration.IntegrationConfigService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

getTypehints :: TypehintRequestDTO -> AppContextM [TypehintIDTO]
getTypehints reqDto =
  runInTransaction $ do
    km <- compileKnowledgeModel reqDto.events reqDto.packageId []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
      ApiLegacyIntegration' integration -> do
        fileIntConfig <- getFileIntegrationConfig integration.iId
        appIntConfig <- getTenantIntegrationConfig integration.iId
        let kmQuestionConfig = question.props
        let encodedUserRequest = M.singleton "q" (encode reqDto.q)
        let userRequest = M.singleton "q" reqDto.q
        let encodedVariables = M.union encodedUserRequest . M.union kmQuestionConfig . M.union appIntConfig $ fileIntConfig
        let variables = M.union userRequest . M.union kmQuestionConfig . M.union appIntConfig $ fileIntConfig
        eiDtos <- retrieveTypehints integration encodedVariables variables
        case eiDtos of
          Right iDtos -> return iDtos
          Left error -> do
            logWarnI _CMP_SERVICE error
            throwError . UserError $ _ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR
      WidgetIntegration' _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION
  where
    getQuestion km questionUuid =
      case M.lookup questionUuid (getQuestionsM km) of
        Just (IntegrationQuestion' question) -> return question
        Just _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__QUESTION_ABSENCE
    getIntegration km integrationUuid =
      case M.lookup integrationUuid (getIntegrationsM km) of
        Just integration -> return integration
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__INTEGRATION_ABSENCE
