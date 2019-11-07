module Api.Handler.Package.PackageHandler where

import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Api.Resource.Package.PackageDetailJM ()
import Api.Resource.Package.PackageSimpleJM ()
import Constant.Api
import Service.Package.PackageService
import Service.PackageBundle.PackageBundleService

getPackagesA :: Endpoint
getPackagesA =
  getMaybeAuthServiceExecutor $ \runInMaybeAuthService -> do
    queryParams <- getListOfQueryParamsIfPresent ["organizationId", "kmId"]
    headers <- getListOfHeaders [xDswUserCountHeaderName, xDswPkgCountHeaderName, xDswQtnCountHeaderName]
    eitherResDtos <- runInMaybeAuthService $ getSimplePackagesFiltered queryParams headers
    case eitherResDtos of
      Right resDtos -> json resDtos
      Left error -> sendError error

getPackageA :: Endpoint
getPackageA = do
  pkgId <- param "pkgId"
  eitherResDto <- runInUnauthService $ getPackageById pkgId
  case eitherResDto of
    Right resDto -> json resDto
    Left error -> sendError error

getPackageBundleA :: Endpoint
getPackageBundleA =
  getAuthServiceExecutor $ \runInAuthService -> do
    pkgId <- param "pkgId"
    eitherResDto <- runInAuthService $ getPackageBundle pkgId
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
