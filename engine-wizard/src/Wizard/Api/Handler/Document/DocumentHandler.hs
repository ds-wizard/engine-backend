module Wizard.Api.Handler.Document.DocumentHandler where

import Control.Lens ((^.))
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (addHeader, json, param, status)

import LensesConfig
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Document.DocumentCreateJM ()
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Service.Document.DocumentService

getDocumentsA :: Endpoint
getDocumentsA =
  checkPermission "DMP_PERM" $ getAuthServiceExecutor $ \runInAuthService -> do
    queryParams <- getListOfQueryParamsIfPresent ["questionnaireUuid"]
    eitherDtos <- runInAuthService $ getDocumentsFiltered queryParams
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postDocumentsA :: Endpoint
postDocumentsA =
  checkPermission "DMP_PERM" $ getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherResDto <- runInAuthService $ createDocument reqDto
      case eitherResDto of
        Left appError -> sendError appError
        Right resDto -> do
          status created201
          json resDto

deleteDocumentA :: Endpoint
deleteDocumentA =
  checkPermission "DMP_PERM" $ getAuthServiceExecutor $ \runInAuthService -> do
    docUuid <- param "docUuid"
    maybeError <- runInAuthService $ deleteDocument docUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

downloadDocumentA :: Endpoint
downloadDocumentA = do
  docUuid <- param "docUuid"
  eitherResult <- runInUnauthService $ downloadDocument docUuid
  case eitherResult of
    Right (doc, body) -> do
      addHeader "Content-Type" (LT.pack . fromMaybe "" $ doc ^. metadata . contentType)
      sendFile (fromMaybe "export" $ doc ^. metadata . fileName) (BSL.fromStrict body)
    Left error -> sendError error
