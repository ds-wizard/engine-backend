module Wizard.Service.Typehint.TypehintService where

import Control.Lens ((^.))
import Data.Map.Strict as M
import Network.URI.Encode (encode)

import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Integration.Http.Typehint.Runner
import Wizard.LensesConfig
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.Config.IntegrationConfigService
import Wizard.Service.Event.EventMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Typehint.TypehintMapper

getTypehints :: TypehintRequestDTO -> AppContextM (Either AppError [TypehintDTO])
getTypehints reqDto =
  heCompileKnowledgeModel (fromDTOs $ reqDto ^. events) (reqDto ^. packageId) [] $ \km ->
    heGetQuestion km (reqDto ^. questionUuid) $ \question ->
      heGetIntegration km (question ^. integrationUuid) $ \integration ->
        heGetIntegrationConfig (integration ^. iId) $ \fileConfig -> do
          let kmQuestionConfig = question ^. props
          let userRequest = (M.singleton "q" (encode $ reqDto ^. q))
          let variables = M.union userRequest . M.union kmQuestionConfig $ fileConfig
          iDtos <- retrieveTypehints integration variables
          return $ fmap (fmap (toDTO (integration ^. itemUrl))) iDtos
  where
    heGetQuestion km questionUuid callback =
      case M.lookup questionUuid (km ^. questionsM) of
        Just (IntegrationQuestion' question) -> callback question
        Just _ -> return . Left . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION
        Nothing -> return . Left . UserError $ _ERROR_VALIDATION__QUESTION_ABSENCE
    heGetIntegration km integrationUuid callback =
      case M.lookup integrationUuid (km ^. integrationsM) of
        Just integration -> callback integration
        Nothing -> return . Left . UserError $ _ERROR_VALIDATION__INTEGRATION_ABSENCE
