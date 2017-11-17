module Api.Handler.Version.VersionHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.Version.VersionDTO
import Context
import DSPConfig
import Service.Package.PackageService

putVersionA :: Context -> DSPConfig -> Scotty.ActionM ()
putVersionA context dspConfig =
  checkPermission context "KM_PUBLISH_PERM" $
  getReqDto $ \reqDto -> do
    kmcUuid <- Scotty.param "kmcUuid"
    version <- Scotty.param "version"
    let description = (reqDto ^. vdtoDescription)
    eitherDto <-
      liftIO $ createPackageFromKMC context kmcUuid version description
    case eitherDto of
      Right dto -> do
        Scotty.status created201
        sendJson dto
      Left error -> sendError error
