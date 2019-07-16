module Api.Handler.IO.IOHandler where

import Data.Aeson (encode)
import qualified Data.List as L
import Data.Text.Lazy
import Network.Wai.Parse
import Web.Scotty.Trans (addHeader, files, json, param, raw)

import Api.Handler.Common
import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.PackageBundle.PackageBundleJM ()
import Service.PackageBundle.PackageBundleService

exportA :: Endpoint
exportA = do
  pId <- param "pId"
  eitherDto <- runInUnauthService $ exportPackageBundle pId
  case eitherDto of
    Right dto -> do
      let cdHeader = "attachment;filename=" ++ pId ++ ".km"
      addHeader "Content-Disposition" (pack cdHeader)
      addHeader "Content-Type" (pack "application/octet-stream")
      raw $ encode dto
    Left error -> sendError error

importA :: Endpoint
importA =
  getAuthServiceExecutor $ \runInAuthService -> do
    fs <- files
    case L.find (\(fieldName, file) -> fieldName == "file") fs of
      Just (fieldName, file) -> do
        let fName = fileName file
        let fContent = fileContent file
        eitherDto <- runInAuthService $ importPackageBundleFromFile fContent
        case eitherDto of
          Right dto -> json dto
          Left error -> sendError error
      Nothing -> notFoundA
