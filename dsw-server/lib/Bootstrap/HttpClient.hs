module Bootstrap.HttpClient where

import Control.Monad.Reader (liftIO)

import Constant.Component
import Integration.Http.Common.HttpClientFactory
import Util.Logger

setupHttpClientManager appConfig = do
  logInfo $ msg _CMP_INTEGRATION "creating http client manager"
  httpClientManager <- liftIO $ createHttpClientManager appConfig
  logInfo $ msg _CMP_INTEGRATION "http client manager successfully created"
  return httpClientManager
