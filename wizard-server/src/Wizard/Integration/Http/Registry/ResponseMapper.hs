module Wizard.Integration.Http.Registry.ResponseMapper where

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client (Response (responseBody))
import Prelude hiding (lookup)

import Shared.Common.Model.Error.Error

toRetrieveKnowledgeModelBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrieveKnowledgeModelBundleByIdResponse = Right . responseBody

toRetrieveTemplateBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrieveTemplateBundleByIdResponse = Right . responseBody

toRetrieveLocaleBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrieveLocaleBundleByIdResponse = Right . responseBody

toUploadDocumentTemplateBundleResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toUploadDocumentTemplateBundleResponse = Right . responseBody

toUploadLocaleBundleResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toUploadLocaleBundleResponse = Right . responseBody
