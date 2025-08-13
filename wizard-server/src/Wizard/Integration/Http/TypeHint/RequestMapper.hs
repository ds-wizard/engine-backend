module Wizard.Integration.Http.TypeHint.RequestMapper (
  toRetrieveLegacyTypeHintsRequest,
  toTypeHintTestRequestError,
  toTypeHintTestRequest,
  toHttpRequest,
) where

import Data.Aeson hiding (encode)
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
import Wizard.Util.Jinja (renderJinjaMultiple, renderJinjaSingle)
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

toTypeHintTestRequestError :: TypeHintRequest -> String -> TypeHintExchange
toTypeHintTestRequestError testRequest message =
  TypeHintExchange
    { request = testRequest
    , response =
        RequestFailedTypeHintResponse' $
          RequestFailedTypeHintResponse
            { message = message
            }
    }

toTypeHintTestRequest :: ApiIntegration -> M.Map String String -> M.Map String String -> String -> IO TypeHintRequest
toTypeHintTestRequest integration variables secrets q = do
  let redactedSecrets = redactSecrets secrets
  let (context, urlContext) = prepareContext variables redactedSecrets q
  (resultUrl, resultHeaders, resultBody) <- renderRequestParts integration context urlContext
  return $
    TypeHintRequest
      { method = integration.requestMethod
      , url = fromRight "" resultUrl
      , headers = fromRight [] resultHeaders
      , body = fromRight (Just "") resultBody
      }

toHttpRequest :: ApiIntegration -> M.Map String String -> M.Map String String -> String -> IO (Either String HttpRequest)
toHttpRequest integration variables secrets q = do
  let (context, urlContext) = prepareContext variables secrets q
  requestParts <- renderRequestParts integration context urlContext
  case requestParts of
    (Left err, _, _) -> return . Left $ "Error when rendering URL: " ++ err
    (_, Left err, _) -> return . Left $ "Error when rendering headers: " ++ err
    (_, _, Left err) -> return $ Left $ "Error when rendering body: " ++ err
    (Right renderedUrl, Right renderedHeaders, Right renderedBody) ->
      return $
        Right $
          HttpRequest
            { requestMethod = integration.requestMethod
            , requestUrl = renderedUrl
            , requestHeaders = mapEntryToMap renderedHeaders
            , requestBody = BS.pack (fromMaybe "" renderedBody)
            , multipart = Nothing
            }

renderRequestParts :: ApiIntegration -> Value -> Value -> IO (Either String String, Either String [MapEntry String String], Either String (Maybe String))
renderRequestParts integration context urlContext = do
  resultUrl <- renderJinjaSingle integration.requestUrl urlContext
  resultHeaders <- renderHttpHeaders integration.requestHeaders context
  resultBody <- case integration.requestBody of
    Just body -> fmap (fmap Just) $ renderJinjaSingle body context
    Nothing -> return $ Right Nothing
  return (resultUrl, resultHeaders, resultBody)

renderHttpHeaders :: [MapEntry String String] -> Value -> IO (Either String [MapEntry String String])
renderHttpHeaders headers context =
  let keys = map (\(MapEntry k _) -> k) headers
      values = map (\(MapEntry _ v) -> v) headers
   in do
        renderedValues <- renderJinjaMultiple values context
        let renderedHeaders = zipWith zipper keys renderedValues
        let (errs, oks) = partitionEithers renderedHeaders
        return $
          if null errs
            then Right oks
            else Left (unlines errs)
  where
    zipper :: String -> Either String String -> Either String (MapEntry String String)
    zipper k (Left err) = Left err
    zipper k (Right v) = Right (MapEntry k v)

-- --------------------------------
-- PRIVATE
-- --------------------------------
redactSecrets :: M.Map String String -> M.Map String String
redactSecrets = M.mapWithKey (\k _ -> "[SECRET:" ++ k ++ "]")

prepareContext :: M.Map String String -> M.Map String String -> String -> (Value, Value)
prepareContext variables secrets q =
  let variablesValue = mapToObject variables
      secretsValue = mapToObject secrets
      encodedQ = encode q
   in ( object ["variables" .= variablesValue, "secrets" .= secretsValue, "q" .= string q]
      , object ["variables" .= variablesValue, "secrets" .= secretsValue, "q" .= string encodedQ]
      )
