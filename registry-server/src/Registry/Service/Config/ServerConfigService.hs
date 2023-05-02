module Registry.Service.Config.ServerConfigService where

import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither)
import System.Environment (lookupEnv)

import Registry.Model.Config.ServerConfig
import Registry.Model.Config.ServerConfigJM ()
import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Error.Error

getServerConfig :: String -> IO (Either AppError ServerConfig)
getServerConfig fileNameBase = do
  mFileNameEnv <- liftIO $ lookupEnv "APPLICATION_CONFIG_PATH"
  let fileName = fromMaybe fileNameBase mFileNameEnv
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . validateServerConfig $ config
    Left error -> return . Left . GeneralServerError . show $ error

validateServerConfig :: ServerConfig -> Either AppError ServerConfig
validateServerConfig config = do
  validateGeneralServerPort config

validateGeneralServerPort :: ServerConfig -> Either AppError ServerConfig
validateGeneralServerPort config
  | port < 1 || port > 65535 = Left . GeneralServerError $ _ERROR_SERVICE_CONFIG__VALIDATION_SERVER_PORT
  | otherwise = Right config
  where
    port = config.general.serverPort
