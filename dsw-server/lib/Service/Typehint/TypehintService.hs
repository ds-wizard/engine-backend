module Service.Typehint.TypehintService where

import Control.Lens ((^.))
import Data.Map.Strict as M
import Network.URI.Encode (encode)

import Api.Resource.Typehint.TypehintDTO
import Api.Resource.Typehint.TypehintRequestDTO
import Integration.Http.Typehint.Runner
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Service.Config.IntegrationConfigService
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelService
import Service.Typehint.TypehintMapper

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
        Just _ -> return . Left . createErrorWithErrorMessage $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION
        Nothing -> return . Left . createErrorWithErrorMessage $ _ERROR_SERVICE_TYPEHINT__NON_EXISTING_QUESTION
    heGetIntegration km integrationUuid callback =
      case M.lookup integrationUuid (km ^. integrationsM) of
        Just integration -> callback integration
        Nothing -> return . Left . createErrorWithErrorMessage $ _ERROR_SERVICE_TYPEHINT__NON_EXISTING_INTEGRATION
