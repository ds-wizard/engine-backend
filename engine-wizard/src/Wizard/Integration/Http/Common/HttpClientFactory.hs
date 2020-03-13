module Wizard.Integration.Http.Common.HttpClientFactory where

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Wizard.Model.Config.ServerConfig

createHttpClientManager :: ServerConfig -> IO Manager
createHttpClientManager serverConfig = newManager tlsManagerSettings
