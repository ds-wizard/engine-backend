module Wizard.Integration.Http.Registry.ResponseMapper
  ( toRetrievePackagesResponse
  , toRetrievePackageBundleByIdResponse
  ) where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Shared.Model.Error.Error
import Wizard.Integration.Http.Common.ResponseMapper
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Integration.Resource.Package.PackageSimpleIJM ()

toRetrievePackagesResponse :: Response BSL.ByteString -> Either AppError [PackageSimpleIDTO]
toRetrievePackagesResponse = deserializeResponseBody

toRetrievePackageBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrievePackageBundleByIdResponse response = Right . getResponseBody $ response
