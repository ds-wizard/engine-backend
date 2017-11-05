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
import Context
import DSPConfig
import Service.Package.PackageService

putVersionA :: Context -> DSPConfig -> Scotty.ActionM ()
putVersionA context dspConfig = do
  kmcUuid <- Scotty.param "kmcUuid"
  version <- Scotty.param "version"
  maybeDto <- liftIO $ createPackageFromKMC context kmcUuid version
  case maybeDto of
    Just dto -> Scotty.json dto
    Nothing -> notFoundA
