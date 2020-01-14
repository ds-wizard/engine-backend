module Wizard.Bootstrap.HttpClient where

import Control.Monad.Reader (liftIO)

import Wizard.Constant.Component
import Wizard.Integration.Http.Common.HttpClientFactory
import Wizard.Util.Logger

setupHttpClientManager appConfig = do
  logInfo $ msg _CMP_INTEGRATION "creating http client manager"
  httpClientManager <- liftIO $ createHttpClientManager appConfig
  logInfo $ msg _CMP_INTEGRATION "http client manager successfully created"
  return httpClientManager
