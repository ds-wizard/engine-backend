module Wizard.Integration.Http.Config.ResponseMapper (
  toCompileClientCssResponse,
) where

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client (Response (responseBody))
import Prelude hiding (lookup)

import Shared.Model.Error.Error

toCompileClientCssResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toCompileClientCssResponse = Right . responseBody
