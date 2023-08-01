module Wizard.Bootstrap.RegistryClient where

import Control.Monad.Reader (liftIO)

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Integration.Http.Common.ServantClient

setupRegistryClient serverConfig httpClientManager = do
  logInfo _CMP_INTEGRATION "creating registry client"
  client <- liftIO $ createRegistryClient serverConfig httpClientManager
  logInfo _CMP_INTEGRATION "registry client successfully created"
  return client
