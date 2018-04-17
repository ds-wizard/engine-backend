module Api.Handler.IO.IOHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.Text.Lazy
import Data.UUID
import Network.Wai.Parse
import Web.Scotty.Trans (addHeader, files, json, param, raw)

import Api.Handler.Common
import Model.Context.AppContext
import Service.Package.PackageService

exportA :: Endpoint
exportA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  pkgId <- param "pkgId"
  eitherDto <- liftIO $ getPackageWithEventsById context pkgId
  case eitherDto of
    Right dto -> do
      let cdHeader = "attachment;filename=" ++ pkgId ++ ".ep"
      addHeader "Content-Disposition" (pack cdHeader)
      addHeader "Content-Type" (pack "application/octet-stream")
      raw $ encode dto
    Left error -> sendError error

importA :: Endpoint
importA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  fs <- files
  case L.find (\(fieldName, file) -> fieldName == "file") fs of
    Just (fieldName, file) -> do
      let fName = fileName file
      let fContent = fileContent file
      eitherDto <- liftIO $ importPackage context fContent
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error
    Nothing -> notFoundA
