module Wizard.Integration.Http.Registry.ResponseMapper where

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client (Response (responseBody))
import Prelude hiding (lookup)

import Shared.Common.Model.Error.Error

toRetrieveKnowledgeModelBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrieveKnowledgeModelBundleByIdResponse = Right . responseBody

toRetrieveDocumentTemplateBundleByCoordinateResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrieveDocumentTemplateBundleByCoordinateResponse = Right . responseBody

toRetrieveLocaleBundleByCoordinateResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrieveLocaleBundleByCoordinateResponse = Right . responseBody

toUploadDocumentTemplateBundleResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toUploadDocumentTemplateBundleResponse = Right . responseBody

toUploadLocaleBundleResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toUploadLocaleBundleResponse = Right . responseBody
