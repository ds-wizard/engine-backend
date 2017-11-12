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
import Api.Resources.Package.PackageDTO
import Context
import DSPConfig
import Service.Package.PackageService

getPackagesA :: Context -> DSPConfig -> Scotty.ActionM ()
getPackagesA context dspConfig = do
  queryParams <- getQueryParams
  dtos <- liftIO $ getSimplePackagesFiltered context queryParams
  sendJson dtos
  where
    getQueryParams = do
      groupId <- getGroupId
      artifactId <- getArtifactId
      return $ maybeToList groupId ++ maybeToList artifactId
      where
        getGroupId = do
          mGroupId <- getQueryParam "groupId"
          case mGroupId of
            Just groupId -> return $ Just ("groupId", groupId)
            Nothing -> return Nothing
        getArtifactId = do
          mArtifactId <- getQueryParam "artifactId"
          case mArtifactId of
            Just artifactId -> return $ Just ("artifactId", artifactId)
            Nothing -> return Nothing

getPackageA :: Context -> DSPConfig -> Scotty.ActionM ()
getPackageA context dspConfig = do
  pkgId <- Scotty.param "pkgId"
  dtos <- liftIO $ getPackageById context pkgId
  sendJson dtos

deletePackagesA :: Context -> DSPConfig -> Scotty.ActionM ()
deletePackagesA context dspConfig = do
  name <- Scotty.param "name"
  isSuccess <- liftIO $ deleteAllPackagesByName context name
  liftIO $ deleteAllPackagesByName context name
  Scotty.status noContent204

deletePackageA :: Context -> DSPConfig -> Scotty.ActionM ()
deletePackageA context dspConfig = do
  pkgId <- Scotty.param "pkgId"
  isSuccess <- liftIO $ deletePackage context pkgId
  if isSuccess
    then Scotty.status noContent204
    else notFoundA
