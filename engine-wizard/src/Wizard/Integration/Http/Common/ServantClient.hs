module Wizard.Integration.Http.Common.ServantClient where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Client (Manager)
import Servant.Client

import Shared.Model.Error.Error
import Wizard.Localization.Messages.Internal
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Util.Logger

createRegistryClient :: ServerConfig -> Manager -> IO ClientEnv
createRegistryClient serverConfig httpClientManager = do
  baseUrl <- parseBaseUrl serverConfig.registry.url
  return $ mkClientEnv httpClientManager baseUrl

runRegistryClient :: ClientM response -> AppContextM response
runRegistryClient request = do
  registryClient <- asks registryClient
  runClient request registryClient

runClient :: ClientM response -> ClientEnv -> AppContextM response
runClient request client = do
  res <- liftIO $ runClientM request client
  case res of
    Right res -> return res
    Left (FailureResponse req res) -> do
      let body = responseBody res
      logErrorU _CMP_INTEGRATION . show $ body
      throwError $ HttpClientError (responseStatusCode res) (BSL.unpack body)
    Left err -> do
      logErrorU _CMP_INTEGRATION . show $ err
      throwError . GeneralServerError $ _ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 502"
