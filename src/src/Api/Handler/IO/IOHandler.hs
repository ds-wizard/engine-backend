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
  eitherDto <- liftIO $ getPackageWithEventsById context pkgId
  case eitherDto of
    Right dto -> do
      let cdHeader = "attachment;filename=" ++ pkgId ++ ".ep"
      Scotty.addHeader "Content-Disposition" (pack cdHeader)
      Scotty.addHeader "Content-Type" (pack "application/octet-stream")
      Scotty.raw $ encode dto
    Left error -> sendError error

importA :: Context -> DSPConfig -> Scotty.ActionM ()
importA context dspConfig = do
  fs <- Scotty.files
  case L.find (\(fieldName, file) -> fieldName == "file") fs of
    Just (fieldName, file) -> do
      let fName = fileName file
      let fContent = fileContent file
      eitherDto <- liftIO $ importPackage context fContent
      case eitherDto of
        Right dto -> sendJson dto
        Left error -> sendError error
    Nothing -> notFoundA
