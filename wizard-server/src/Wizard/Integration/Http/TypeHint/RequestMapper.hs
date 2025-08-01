module Wizard.Integration.Http.TypeHint.RequestMapper (
  toRetrieveLegacyTypeHintsRequest,
  toTypeHintTestRequestError,
  renderTypeHintTestRequest,
  renderHttpRequest,
) where

import Control.Monad (forM)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromRight, partitionEithers)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Network.URI.Encode (encode)
import Prelude hiding (lookup)

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Model.Http.HttpRequest
import Shared.Common.Util.Aeson as A
import Wizard.Util.Interpolation (interpolateMapValues, interpolateString)
import Wizard.Util.Jinja (renderJinjaSingle)
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

toRetrieveLegacyTypeHintsRequest :: ApiLegacyIntegration -> M.Map String String -> M.Map String String -> HttpRequest
toRetrieveLegacyTypeHintsRequest intConfig encodedVariables variables =
  HttpRequest
    { requestMethod = intConfig.requestMethod
    , requestUrl = interpolateString encodedVariables intConfig.requestUrl
    , requestHeaders = interpolateMapValues variables (mapEntryToMap intConfig.requestHeaders)
    , requestBody = BS.pack $ interpolateString variables intConfig.requestBody
    , multipart = Nothing
    }

toTypeHintTestRequestError :: TypeHintResponseRequest -> TypeHintResponse
toTypeHintTestRequestError testRequest =
  TypeHintResponse
    { request = testRequest
    , response =
        TypeHintResponseResponse
          { responseType = RequestFailedTypeHintResponse
          , status = Nothing
          , contentType = Nothing
          , body = Nothing
          }
    }

renderTypeHintTestRequest :: ApiIntegration -> M.Map String String -> IO TypeHintResponseRequest
renderTypeHintTestRequest intConfig variables = do
  let urlEncodedVariables = M.map encode variables
  let encodedVariablesValue = mapToObject urlEncodedVariables
  let variablesValue = mapToObject variables
  resultUrl <- renderJinjaSingle intConfig.requestUrl encodedVariablesValue
  resultBody <- case intConfig.requestBody of
    Just body -> fmap (fmap Just) $ renderJinjaSingle body variablesValue
    Nothing -> return $ Right Nothing
  resultHeaders <- renderHttpHeadersTest intConfig.requestHeaders variablesValue
  return $
    TypeHintResponseRequest
      { method = intConfig.requestMethod
      , url = fromRight "" resultUrl
      , headers = resultHeaders
      , body = fromRight (Just "") resultBody
      }

renderHttpRequest :: ApiIntegration -> M.Map String String -> IO (Either String HttpRequest)
renderHttpRequest intConfig variables = do
  let urlEncodedVariables = M.map encode variables
  let encodedVariablesValue = mapToObject urlEncodedVariables
  let variablesValue = mapToObject variables
  resultUrl <- renderJinjaSingle intConfig.requestUrl encodedVariablesValue
  resultBody <- renderJinjaSingle (fromMaybe "" intConfig.requestBody) variablesValue
  resultHeaders <- renderHttpHeaders intConfig.requestHeaders variablesValue
  case (resultUrl, resultHeaders, resultBody) of
    (Left err, _, _) -> return $ Left err
    (_, Left err, _) -> return $ Left err
    (_, _, Left err) -> return $ Left err
    (Right renderedUrl, Right renderedHeaders, Right renderedBody) ->
      return $
        Right $
          HttpRequest
            { requestMethod = intConfig.requestMethod
            , requestUrl = renderedUrl
            , requestHeaders = mapEntryToMap renderedHeaders
            , requestBody = BS.pack renderedBody
            , multipart = Nothing
            }

renderHttpHeaders :: [MapEntry String String] -> Value -> IO (Either String [MapEntry String String])
renderHttpHeaders headers variablesValue = do
  results <- forM headers $ \(MapEntry k v) -> do
    rendered <- renderJinjaSingle v variablesValue
    return $ fmap (MapEntry k) rendered
  let (errs, oks) = partitionEithers results
  return $
    if null errs
      then Right oks
      else Left (unlines errs)

renderHttpHeadersTest :: [MapEntry String String] -> Value -> IO [MapEntry String String]
renderHttpHeadersTest headers variablesValue = do
  forM headers $ \(MapEntry k v) -> do
    rendered <- renderJinjaSingle v variablesValue
    return $ MapEntry k (fromRight "" rendered)
