module Wizard.Integration.Http.Typehint.RequestMapper (
  toRetrieveTypehintsRequest,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Model.Http.HttpRequest
import Wizard.Util.Interpolation (interpolateMapValues, interpolateString)
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

toRetrieveTypehintsRequest :: ApiIntegration -> M.Map String String -> M.Map String String -> HttpRequest
toRetrieveTypehintsRequest intConfig encodedVariables variables =
  HttpRequest
    { requestMethod = intConfig.requestMethod
    , requestUrl = interpolateString encodedVariables intConfig.requestUrl
    , requestHeaders = interpolateMapValues variables (mapEntryToMap intConfig.requestHeaders)
    , requestBody = BS.pack $ interpolateString variables intConfig.requestBody
    , multipart = Nothing
    }
