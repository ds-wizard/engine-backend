module Api.Handler.Package.PackageHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe
import Data.Text
import Data.UUID
import Network.HTTP.Types.Status (noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Package.PackageDTO
import Model.Context.AppContext
import Service.Package.PackageService

getPackagesA :: Endpoint
getPackagesA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["groupId", "artifactId"]
    eitherResDtos <- liftIO $ getPackagesFiltered context queryParams
    case eitherResDtos of
      Right resDtos -> json resDtos
      Left error -> sendError error

getUniquePackagesA :: Endpoint
getUniquePackagesA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["groupId", "artifactId"]
    eitherResDtos <- liftIO $ getSimplePackagesFiltered context queryParams
    case eitherResDtos of
      Right resDtos -> json resDtos
      Left error -> sendError error

getPackageA :: Endpoint
getPackageA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "PM_PERM" $ do
    pkgId <- param "pkgId"
    eitherResDto <- liftIO $ getPackageById context pkgId
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error

deletePackagesA :: Endpoint
deletePackagesA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["groupId", "artifactId"]
    maybeError <- liftIO $ deletePackagesByQueryParams context queryParams
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

deletePackageA :: Endpoint
deletePackageA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "PM_PERM" $ do
    pkgId <- param "pkgId"
    maybeError <- liftIO $ deletePackage context pkgId
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
