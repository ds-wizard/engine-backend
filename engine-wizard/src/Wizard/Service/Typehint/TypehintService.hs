module Wizard.Service.Typehint.TypehintService where

import Control.Monad.Except (throwError)
import Data.Map.Strict as M
import Network.URI.Encode (encode)

import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Typehint.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.IntegrationConfigService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Typehint.TypehintMapper
import Wizard.Util.Logger

getTypehints :: TypehintRequestDTO -> AppContextM [TypehintDTO]
getTypehints reqDto =
  runInTransaction $ do
    km <- compileKnowledgeModel reqDto.events reqDto.packageId []
    question <- getQuestion km reqDto.questionUuid
    integration' <- getIntegration km question.integrationUuid
    case integration' of
      ApiIntegration' integration -> do
        fileIntConfig <- getFileIntegrationConfig integration.iId
        appIntConfig <- getAppIntegrationConfig integration.iId
        let kmQuestionConfig = question.props
        let userRequest = M.singleton "q" (encode reqDto.q)
        let variables = M.union userRequest . M.union kmQuestionConfig . M.union appIntConfig $ fileIntConfig
        eiDtos <- retrieveTypehints integration variables
        case eiDtos of
          Right iDtos -> return . fmap (toDTO integration.itemUrl) $ iDtos
          Left error -> do
            logWarnU _CMP_SERVICE error
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
