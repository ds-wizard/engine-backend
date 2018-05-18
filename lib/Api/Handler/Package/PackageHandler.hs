module Api.Handler.Package.PackageHandler where

import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Package.PackageDTO ()
import Service.Package.PackageService

getPackagesA :: Endpoint
getPackagesA =
  checkPermission "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["organizationId", "kmId"]
    eitherResDtos <- lift $ getPackagesFiltered queryParams
    case eitherResDtos of
      Right resDtos -> json resDtos
      Left error -> sendError error

getUniquePackagesA :: Endpoint
getUniquePackagesA =
  checkPermission "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["organizationId", "kmId"]
    eitherResDtos <- lift $ getSimplePackagesFiltered queryParams
    case eitherResDtos of
      Right resDtos -> json resDtos
      Left error -> sendError error

getPackageA :: Endpoint
getPackageA =
  checkPermission "PM_PERM" $ do
    pkgId <- param "pkgId"
    eitherResDto <- lift $ getPackageById pkgId
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error

deletePackagesA :: Endpoint
deletePackagesA =
  checkPermission "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["organizationId", "kmId"]
    maybeError <- lift $ deletePackagesByQueryParams queryParams
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

deletePackageA :: Endpoint
deletePackageA =
  checkPermission "PM_PERM" $ do
    pkgId <- param "pkgId"
    maybeError <- lift $ deletePackage pkgId
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
