module Wizard.Integration.Http.TypeHint.Runner (
  retrieveLegacyTypeHints,
  retrieveTypeHints,
  testRetrieveTypeHints,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict as M

import Shared.Common.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.TypeHint.RequestMapper
import Wizard.Integration.Http.TypeHint.ResponseMapper
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

retrieveLegacyTypeHints :: ApiLegacyIntegration -> M.Map String String -> M.Map String String -> AppContextM (Either String [TypeHintLegacyIDTO])
retrieveLegacyTypeHints integrationConfig encodedVariables variables =
  runRequest' (toRetrieveLegacyTypeHintsRequest integrationConfig encodedVariables variables) (toRetrieveLegacyTypeHintsResponse integrationConfig)

retrieveTypeHints :: ApiIntegration -> M.Map String String -> AppContextM (Either String [TypeHintIDTO])
retrieveTypeHints integrationConfig variables = do
  httpRequest <- liftIO $ renderHttpRequest integrationConfig variables
  case httpRequest of
    Left error -> return $ Left error
    Right request -> runRequestIO' request (toRetrieveTypeHintsResponse integrationConfig)

testRetrieveTypeHints :: ApiIntegration -> M.Map String String -> AppContextM TypeHintResponse
testRetrieveTypeHints integrationConfig variables = do
  httpRequest <- liftIO $ renderHttpRequest integrationConfig variables
  testRequest <- liftIO $ renderTypeHintTestRequest integrationConfig variables
  case httpRequest of
    Left error -> return $ toTypeHintTestRequestError testRequest
    Right request -> do
      httpResponse <- runSimpleRequest request
      case httpResponse of
        Left error -> return $ toTypeHintTestResponseError testRequest
        Right response -> return $ toTypeHintTestResponse testRequest response
