module Api.Handler.IO.IOHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import Network.Wai.Parse
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Context
import DSPConfig
import Service.Package.PackageService

exportA :: Context -> DSPConfig -> Scotty.ActionM ()
exportA context dspConfig = do
  pkgId <- Scotty.param "pkgId"
  maybeDto <- liftIO $ getPackageWithEventsById context pkgId
  case maybeDto of
    Just dto -> do
      let cdHeader = "attachment;filename=" ++ pkgId ++ ".ep"
      Scotty.addHeader "Content-Disposition" (pack cdHeader)
      Scotty.addHeader "Content-Type" (pack "application/octet-stream")
      Scotty.raw $ encode dto
    Nothing -> notFoundA

importA :: Context -> DSPConfig -> Scotty.ActionM ()
importA context dspConfig = do
  fs <- Scotty.files
  case L.find (\(fieldName, file) -> fieldName == "file") fs of
    Just (fieldName, file) -> do
      let fName = fileName file
      let fContent = fileContent file
      maybeDto <- liftIO $ importPackage context fContent
      case maybeDto of
        Just dto -> sendJson dto
        Nothing -> notFoundA
    Nothing -> notFoundA
