module Wizard.Api.Handler.Version.VersionHandler where

import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (json, param, status)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Version.VersionJM ()
import Wizard.Service.Version.VersionService

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
