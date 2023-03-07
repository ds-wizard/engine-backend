module Wizard.Integration.Http.Registry.ResponseMapper where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Shared.Model.Error.Error
import Wizard.Integration.Http.Common.ResponseMapper

toRetrievePackageBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrievePackageBundleByIdResponse = Right . getResponseBody

toRetrieveTemplateBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrieveTemplateBundleByIdResponse = Right . getResponseBody

toRetrieveLocaleBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrieveLocaleBundleByIdResponse = Right . getResponseBody

toUploadDocumentTemplateBundleResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toUploadDocumentTemplateBundleResponse = Right . getResponseBody

toUploadLocaleBundleResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toUploadLocaleBundleResponse = Right . getResponseBody
