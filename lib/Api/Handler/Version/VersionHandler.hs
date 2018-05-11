module Api.Handler.Version.VersionHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Version.VersionDTO
import Model.Context.AppContext
import Service.Package.PackageService

putVersionA :: Endpoint
putVersionA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PUBLISH_PERM" $
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      version <- param "version"
      let description = (reqDto ^. vdtoDescription)
      eitherDto <- liftIO $ createPackageFromKMC context branchUuid version description
      case eitherDto of
        Right dto -> do
          status created201
          json dto
        Left error -> sendError error
