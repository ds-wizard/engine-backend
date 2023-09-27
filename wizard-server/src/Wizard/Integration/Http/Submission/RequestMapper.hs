module Wizard.Integration.Http.Submission.RequestMapper (
  toUploadDocumentRequest,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Common.Model.Http.HttpRequest
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Util.Interpolation (interpolateMapValues, interpolateString)

toUploadDocumentRequest :: TenantConfigSubmissionServiceRequest -> M.Map String String -> BS.ByteString -> HttpRequest
toUploadDocumentRequest req variables reqBody =
  HttpRequest
    { requestMethod = req.method
    , requestUrl = interpolateString variables req.url
    , requestHeaders = interpolateMapValues variables req.headers
    , requestBody = reqBody
    , multipart =
        if req.multipart.enabled
          then Just HttpRequestMultipart {key = req.multipart.fileName, fileName = Nothing, contentType = Nothing}
          else Nothing
    }
