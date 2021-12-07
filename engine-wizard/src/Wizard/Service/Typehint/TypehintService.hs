module Wizard.Service.Typehint.TypehintService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Data.Map.Strict as M
import Network.URI.Encode (encode)

import LensesConfig
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

getTypehints :: TypehintRequestDTO -> AppContextM [TypehintDTO]
getTypehints reqDto =
  runInTransaction $ do
    km <- compileKnowledgeModel (reqDto ^. events) (reqDto ^. packageId) []
    question <- getQuestion km (reqDto ^. questionUuid)
    integration <- getIntegration km (question ^. integrationUuid)
    fileConfig <- getIntegrationConfig (integration ^. iId)
    let kmQuestionConfig = question ^. props
    let userRequest = M.singleton "q" (encode $ reqDto ^. q)
    let variables = M.union userRequest . M.union kmQuestionConfig $ fileConfig
    iDtos <- retrieveTypehints integration variables
    return . fmap (toDTO (integration ^. responseItemUrl)) $ iDtos
  where
    getQuestion km questionUuid =
      case M.lookup questionUuid (km ^. questionsM) of
        Just (IntegrationQuestion' question) -> return question
        Just _ -> throwError . UserError $ _ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__QUESTION_ABSENCE
    getIntegration km integrationUuid =
      case M.lookup integrationUuid (km ^. integrationsM) of
        Just integration -> return integration
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__INTEGRATION_ABSENCE
