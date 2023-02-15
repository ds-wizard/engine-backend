module Wizard.Integration.Http.Typehint.RequestMapper (
  toRetrieveTypehintsRequest,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Http.HttpRequest
import Wizard.Util.Interpolation (interpolateMapValues, interpolateString)

toRetrieveTypehintsRequest :: ApiIntegration -> M.Map String String -> HttpRequest
toRetrieveTypehintsRequest intConfig variables =
  HttpRequest
    { requestMethod = intConfig.requestMethod
    , requestUrl = interpolateString variables intConfig.requestUrl
    , requestHeaders = interpolateMapValues variables (mapEntryToMap intConfig.requestHeaders)
    , requestBody = BS.pack $ interpolateString variables intConfig.requestBody
    , multipart = Nothing
    }
