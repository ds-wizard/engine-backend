module Shared.Common.Api.Middleware.OptionsMiddleware where

import Network.HTTP.Types
import Network.Wai

optionsMiddleware :: Middleware
optionsMiddleware app req cb =
  case requestMethod req of
    "OPTIONS" -> cb (responseBuilder (Status 200 "OK") [] mempty)
    _ -> app req cb
