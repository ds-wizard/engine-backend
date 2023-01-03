module Wizard.Service.Config.Server.ServerConfigService where

import Data.Yaml (decodeFileEither)

import Shared.Localization.Messages.Internal
import Shared.Model.Config.ServerConfigIM
import Shared.Model.Error.Error
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigIM ()
import Wizard.Model.Config.ServerConfigJM ()

getServerConfig :: String -> IO (Either AppError ServerConfig)
getServerConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> do
      updatedConfig <- applyEnv config
      return . validateServerConfig $ updatedConfig
    Left error -> return . Left . GeneralServerError . show $ error

validateServerConfig :: ServerConfig -> Either AppError ServerConfig
validateServerConfig config = do
  validateGeneralSecret config
  validateGeneralServerPort config

validateGeneralSecret :: ServerConfig -> Either AppError ServerConfig
validateGeneralSecret config
  | secretLen /= 32 = Left . GeneralServerError $ _ERROR_SERVICE_CONFIG__VALIDATION_SECRET
  | otherwise = Right config
  where
    secretLen = length $ config.general.secret

validateGeneralServerPort :: ServerConfig -> Either AppError ServerConfig
validateGeneralServerPort config
  | port < 1 || port > 65535 = Left . GeneralServerError $ _ERROR_SERVICE_CONFIG__VALIDATION_SERVER_PORT
  | otherwise = Right config
  where
    port = config.general.serverPort
