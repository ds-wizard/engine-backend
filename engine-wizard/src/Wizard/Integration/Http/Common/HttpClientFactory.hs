module Wizard.Integration.Http.Common.HttpClientFactory where

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Wizard.Model.Config.AppConfig

createHttpClientManager :: AppConfig -> IO Manager
createHttpClientManager appConfig = newManager tlsManagerSettings
