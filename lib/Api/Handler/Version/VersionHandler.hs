module Api.Handler.Version.VersionHandler where

import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.Version.VersionJM ()
import Service.Version.VersionService

putVersionA :: Endpoint
putVersionA =
  checkPermission "KM_PUBLISH_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      version <- param "version"
      eitherDto <- runInAuthService $ publishPackage branchUuid version reqDto
      case eitherDto of
        Right dto -> do
          status created201
          json dto
        Left error -> sendError error
