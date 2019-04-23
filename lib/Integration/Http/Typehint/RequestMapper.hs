module Integration.Http.Typehint.RequestMapper
  ( toRetrieveTypehintsRequest
  ) where

import Control.Lens ((^.))
import Data.Map.Strict as M
import Prelude hiding (lookup)

import LensesConfig
import Model.Http.HttpRequest
import Model.KnowledgeModel.KnowledgeModel
import Util.Interpolation (interpolateMapValues, interpolateString)

toRetrieveTypehintsRequest :: Integration -> M.Map String String -> HttpRequest
toRetrieveTypehintsRequest intConfig variables =
  HttpRequest
  { _httpRequestRequestMethod = intConfig ^. requestMethod
  , _httpRequestRequestUrl = interpolateString variables (intConfig ^. requestUrl)
  , _httpRequestRequestHeaders = interpolateMapValues variables (intConfig ^. requestHeaders)
  , _httpRequestRequestBody = interpolateString variables (intConfig ^. requestBody)
  }
