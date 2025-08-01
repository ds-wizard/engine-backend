module Wizard.Integration.Http.TypeHint.ResponseMapper (
  toRetrieveLegacyTypeHintsResponse,
  toRetrieveTypeHintsResponse,
  toTypeHintTestResponse,
  toRetrieveTypeHintsTestResponse,
  toTypeHintTestResponseError,
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Either (rights)
import qualified Data.HashMap.Strict as HM
import Data.List (lookup)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Client (responseBody, responseHeaders, responseStatus)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Shared.Common.Integration.Http.Common.ResponseMapper
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Ginger
import Shared.Common.Util.String (splitOn)
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Util.Jinja (renderJinjaBatch)
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

toRetrieveLegacyTypeHintsResponse :: ApiLegacyIntegration -> Response BSL.ByteString -> Either String [TypeHintLegacyIDTO]
toRetrieveLegacyTypeHintsResponse intConfig response =
  case extractResponseBody response >>= extractNestedField listField >>= convertToArray >>= mapRecords of
    Right dto -> Right dto
    Left error -> Left . show $ error
  where
    listField =
      case intConfig.responseListField of
        Just responseListField -> splitOn "." responseListField
        Nothing -> []
    mapRecords :: [Value] -> Either AppError [TypeHintLegacyIDTO]
    mapRecords = Right . rights . fmap mapRecord
    mapRecord :: Value -> Either String TypeHintLegacyIDTO
    mapRecord record = do
      let contextMap = HM.fromList [("item", record)]
      itemId <-
        case intConfig.responseItemId of
          Just responseItemId -> do
            result <- renderEither responseItemId contextMap
            Right . Just . T.unpack $ result
          Nothing -> Right Nothing
      itemTemplate <- renderEither intConfig.responseItemTemplate contextMap
      Right $ TypeHintLegacyIDTO {intId = itemId, name = T.unpack itemTemplate}

toTypeHintTestResponseError :: TypeHintResponseRequest -> TypeHintResponse
toTypeHintTestResponseError testRequest =
  TypeHintResponse
    { request = testRequest
    , response =
        TypeHintResponseResponse
          { responseType = RemoteErrorTypeHintResponse
          , status = Nothing
          , contentType = Nothing
          , body = Nothing
          }
    }

toTypeHintTestResponse :: TypeHintResponseRequest -> Response BSL.ByteString -> TypeHintResponse
toTypeHintTestResponse testRequest httpResponse =
  TypeHintResponse
    { request = testRequest
    , response =
        TypeHintResponseResponse
          { responseType = toTypeHintResponseType status
          , status = Just status
          , contentType = contentType
          , body = Just body
          }
    }
  where
    status = statusCode . responseStatus $ httpResponse
    contentType = fmap (T.unpack . T.decodeUtf8) $ lookup hContentType (responseHeaders httpResponse)
    body = TL.unpack . TL.decodeUtf8 $ responseBody httpResponse

toTypeHintResponseType :: Int -> TypeHintResponseResponseType
toTypeHintResponseType status
  | status >= 200 && status < 300 = SuccessTypeHintResponse
  | otherwise = RemoteErrorTypeHintResponse

toRetrieveTypeHintsResponse :: ApiIntegration -> Response BSL.ByteString -> IO (Either String [TypeHintIDTO])
toRetrieveTypeHintsResponse intConfig response = do
  case extractResponseBody response >>= extractNestedField listField >>= convertToArray of
    Left err -> return . Left . show $ err
    Right records' -> do
      let recordsInItem = fmap (\v -> object ["item" .= v]) records'
      values <- renderJinjaBatch intConfig.responseItemTemplate recordsInItem
      valuesForSelect <- case intConfig.responseItemTemplateForSelection of
        Just templateForSelection -> (fmap . fmap . fmap $ Just) $ renderJinjaBatch templateForSelection recordsInItem
        Nothing -> return $ fmap (const (Right Nothing)) recordsInItem
      return . Right . rights . fmap mapRecord $ zip3 valuesForSelect values records'
  where
    listField =
      case intConfig.responseListField of
        Just responseListField -> splitOn "." responseListField
        Nothing -> []
    mapRecord :: (Either String (Maybe String), Either String String, Value) -> Either String TypeHintIDTO
    mapRecord (Left err, _, _) = Left err
    mapRecord (_, Left err, _) = Left err
    mapRecord (Right valueForSelection, Right value, record) =
      Right $ TypeHintIDTO {valueForSelection = valueForSelection, value = value, raw = record}

toRetrieveTypeHintsTestResponse :: ApiIntegration -> Response BSL.ByteString -> IO (Either String TypeHintResponseResponse)
toRetrieveTypeHintsTestResponse intConfig response = undefined
