module Wizard.Integration.Http.Config.ResponseMapper
  ( toCompileClientCssResponse
  ) where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Shared.Model.Error.Error
import Wizard.Integration.Http.Common.ResponseMapper

toCompileClientCssResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toCompileClientCssResponse = extractResponseBodyRaw
