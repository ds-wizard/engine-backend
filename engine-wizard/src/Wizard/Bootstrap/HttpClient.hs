module Wizard.Bootstrap.HttpClient where

import Control.Monad.Reader (liftIO)

import Shared.Constant.Component
import Wizard.Integration.Http.Common.HttpClientFactory
import Wizard.Util.Logger

setupHttpClientManager serverConfig = do
  logInfo _CMP_INTEGRATION "creating http client manager"
  httpClientManager <- liftIO $ createHttpClientManager serverConfig
  logInfo _CMP_INTEGRATION "http client manager successfully created"
  return httpClientManager
