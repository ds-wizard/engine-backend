module Shared.Common.Bootstrap.HttpClient where

import Control.Monad.Reader (liftIO)

import Shared.Common.Constant.Component
import Shared.Common.Integration.Http.Common.HttpClientFactory
import Shared.Common.Util.Logger

setupHttpClientManager serverConfig = do
  logInfo _CMP_INTEGRATION "creating http client manager"
  httpClientManager <- liftIO $ createHttpClientManager serverConfig
  logInfo _CMP_INTEGRATION "http client manager successfully created"
  return httpClientManager
