module Api.Handler.Package.PackageHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.Package.PackageDTO
import Context
import DSPConfig
import Service.Package.PackageService

getPackagesA :: Context -> DSPConfig -> Scotty.ActionM ()
getPackagesA context dspConfig = do
  dtos <- liftIO $ getAllPackages context
  sendJson dtos

getPackageA :: Context -> DSPConfig -> Scotty.ActionM ()
getPackageA context dspConfig = do
  name <- Scotty.param "name"
  dtos <- liftIO $ getPackagesForName context name
  sendJson dtos

deletePackagesByNameA :: Context -> DSPConfig -> Scotty.ActionM ()
deletePackagesByNameA context dspConfig = do
  name <- Scotty.param "name"
  isSuccess <- liftIO $ deleteAllPackagesByName context name
  liftIO $ deleteAllPackagesByName context name
  Scotty.status noContent204

deletePackageA :: Context -> DSPConfig -> Scotty.ActionM ()
deletePackageA context dspConfig = do
  name <- Scotty.param "name"
  version <- Scotty.param "version"
  isSuccess <- liftIO $ deletePackage context name version
  if isSuccess
    then Scotty.status noContent204
    else notFoundA
