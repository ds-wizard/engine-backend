module Wizard.Integration.Http.Submission.RequestMapper
  ( toUploadDocumentRequest
  ) where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict as M
import Prelude hiding (lookup)

import LensesConfig
import Wizard.Model.Config.AppConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Util.Interpolation (interpolateMapValues, interpolateString)

toUploadDocumentRequest :: AppConfigSubmissionServiceRequest -> M.Map String String -> BS.ByteString -> HttpRequest
toUploadDocumentRequest req variables reqBody =
  HttpRequest
    { _httpRequestRequestMethod = req ^. method
    , _httpRequestRequestUrl = interpolateString variables (req ^. url)
    , _httpRequestRequestHeaders = interpolateMapValues variables (req ^. headers)
    , _httpRequestRequestBody = reqBody
    , _httpRequestMultipartFileName =
        if req ^. multipart . enabled
          then Just $ req ^. multipart . fileName
          else Nothing
    }
