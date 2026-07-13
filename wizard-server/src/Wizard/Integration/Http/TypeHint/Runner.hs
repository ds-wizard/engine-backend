module Wizard.Integration.Http.TypeHint.Runner (
  retrieveTypeHints,
  testRetrieveTypeHints,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Map.Strict as M

import Shared.Common.Integration.Http.Common.HttpClient
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Integration.Http.TypeHint.RequestMapper
import Wizard.Integration.Http.TypeHint.ResponseMapper
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

retrieveTypeHints :: ApiIntegration -> M.Map String String -> M.Map String String -> String -> AppContextM (Either String [TypeHintIDTO])
retrieveTypeHints integration variables secrets q = do
  httpRequest <- liftIO $ toHttpRequest integration variables secrets q
  restrictedHttpClientManager <- asks (.restrictedHttpClientManager')
  case httpRequest of
    Left error -> return $ Left error
    Right request -> runRequestIO'With restrictedHttpClientManager request (toRetrieveTypeHintsResponse integration)

testRetrieveTypeHints :: ApiIntegration -> M.Map String String -> M.Map String String -> String -> AppContextM TypeHintExchange
testRetrieveTypeHints integration variables secrets q = do
  httpRequest <- liftIO $ toHttpRequest integration variables secrets q
  testRequest <- liftIO $ toTypeHintTestRequest integration variables secrets q
  restrictedHttpClientManager <- asks (.restrictedHttpClientManager')
  case httpRequest of
    Left error -> return $ toTypeHintTestRequestError testRequest error
    Right request -> do
      httpResponse <- runSimpleRequestWith restrictedHttpClientManager request
      case httpResponse of
        Left error -> return $ toTypeHintTestResponseError testRequest (toResponseErrorMessage error)
        Right response -> return $ toTypeHintTestResponse testRequest response
