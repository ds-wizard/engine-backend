module Integration.Http.Registry.ResponseMapper
  ( toRetrievePackagesResponse
  , toRetrievePackageBundleByIdResponse
  ) where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Integration.Http.Common.ResponseMapper
import Integration.Resource.Package.PackageSimpleIDTO
import Integration.Resource.Package.PackageSimpleIJM ()
import Model.Error.Error

toRetrievePackagesResponse :: Response BSL.ByteString -> Either AppError [PackageSimpleIDTO]
toRetrievePackagesResponse = deserializeResponseBody

toRetrievePackageBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrievePackageBundleByIdResponse response = Right . getResponseBody $ response
