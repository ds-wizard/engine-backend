module Api.Handler.Package.PackageHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.Package.PackageDTO
import Common.Context
import Model.Config.DSWConfig
import Service.Package.PackageService

getPackagesA :: Context -> DSWConfig -> Scotty.ActionM ()
getPackagesA context dswConfig =
  checkPermission context "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["groupId", "artifactId"]
    eitherResDtos <- liftIO $ getPackagesFiltered context queryParams
    case eitherResDtos of
      Right resDtos -> sendJson resDtos
      Left error -> sendError error

getUniquePackagesA :: Context -> DSWConfig -> Scotty.ActionM ()
getUniquePackagesA context dswConfig =
  checkPermission context "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["groupId", "artifactId"]
    eitherResDtos <- liftIO $ getSimplePackagesFiltered context queryParams
    case eitherResDtos of
      Right resDtos -> sendJson resDtos
      Left error -> sendError error

getPackageA :: Context -> DSWConfig -> Scotty.ActionM ()
getPackageA context dswConfig =
  checkPermission context "PM_PERM" $ do
    pkgId <- Scotty.param "pkgId"
    eitherResDto <- liftIO $ getPackageById context pkgId
    case eitherResDto of
      Right resDto -> sendJson resDto
      Left error -> sendError error

deletePackagesA :: Context -> DSWConfig -> Scotty.ActionM ()
deletePackagesA context dswConfig =
  checkPermission context "PM_PERM" $ do
    queryParams <- getListOfQueryParamsIfPresent ["groupId", "artifactId"]
    maybeError <- liftIO $ deletePackagesByQueryParams context queryParams
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error

deletePackageA :: Context -> DSWConfig -> Scotty.ActionM ()
deletePackageA context dswConfig =
  checkPermission context "PM_PERM" $ do
    pkgId <- Scotty.param "pkgId"
    maybeError <- liftIO $ deletePackage context pkgId
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
