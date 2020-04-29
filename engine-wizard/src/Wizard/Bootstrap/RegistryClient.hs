module Wizard.Bootstrap.RegistryClient where

import Control.Monad.Reader (liftIO)

import Wizard.Constant.Component
import Wizard.Integration.Http.Common.ServantClient
import Wizard.Util.Logger

setupRegistryClient serverConfig httpClientManager = do
  logInfo _CMP_INTEGRATION "creating registry client"
  client <- liftIO $ createRegistryClient serverConfig httpClientManager
  logInfo _CMP_INTEGRATION "registry client successfully created"
  return client
