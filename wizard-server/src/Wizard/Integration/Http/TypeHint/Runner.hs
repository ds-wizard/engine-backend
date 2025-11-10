module Wizard.Integration.Http.TypeHint.Runner (
  retrieveLegacyTypeHints,
  retrieveTypeHints,
  testRetrieveTypeHints,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict as M

import Shared.Common.Integration.Http.Common.HttpClient
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Integration.Http.TypeHint.RequestMapper
import Wizard.Integration.Http.TypeHint.ResponseMapper
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

retrieveLegacyTypeHints :: ApiLegacyIntegration -> M.Map String String -> M.Map String String -> AppContextM (Either String [TypeHintLegacyIDTO])
retrieveLegacyTypeHints intConfig encodedVariables variables =
  runRequest' (toRetrieveLegacyTypeHintsRequest intConfig encodedVariables variables) (toRetrieveLegacyTypeHintsResponse intConfig)

retrieveTypeHints :: ApiIntegration -> M.Map String String -> M.Map String String -> String -> AppContextM (Either String [TypeHintIDTO])
retrieveTypeHints integration variables secrets q = do
  httpRequest <- liftIO $ toHttpRequest integration variables secrets q
  case httpRequest of
    Left error -> return $ Left error
    Right request -> runRequestIO' request (toRetrieveTypeHintsResponse integration)

testRetrieveTypeHints :: ApiIntegration -> M.Map String String -> M.Map String String -> String -> AppContextM TypeHintExchange
testRetrieveTypeHints integration variables secrets q = do
  httpRequest <- liftIO $ toHttpRequest integration variables secrets q
  testRequest <- liftIO $ toTypeHintTestRequest integration variables secrets q
  case httpRequest of
    Left error -> return $ toTypeHintTestRequestError testRequest error
    Right request -> do
      httpResponse <- runSimpleRequest request
      case httpResponse of
        Left error -> return $ toTypeHintTestResponseError testRequest (toResponseErrorMessage error)
        Right response -> return $ toTypeHintTestResponse testRequest response
