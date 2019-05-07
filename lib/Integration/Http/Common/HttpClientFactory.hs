module Integration.Http.Common.HttpClientFactory where

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Model.Config.AppConfig

createHttpClientManager :: AppConfig -> IO Manager
createHttpClientManager dswConfig = newManager tlsManagerSettings
