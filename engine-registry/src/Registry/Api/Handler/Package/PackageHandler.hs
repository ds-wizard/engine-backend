module Registry.Api.Handler.Package.PackageHandler where

import Web.Scotty.Trans (json, param)

import Registry.Api.Handler.Common
import Registry.Api.Resource.Package.PackageDetailJM ()
import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Service.Package.PackageService
import Registry.Service.PackageBundle.PackageBundleService
import Shared.Constant.Api

getPackagesA :: Endpoint
getPackagesA =
  getMaybeAuthServiceExecutor $ \runInMaybeAuthService -> do
    queryParams <- getListOfQueryParamsIfPresent ["organizationId", "kmId"]
    headers <- getListOfHeaders [xUserCountHeaderName, xPkgCountHeaderName, xQtnCountHeaderName]
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
