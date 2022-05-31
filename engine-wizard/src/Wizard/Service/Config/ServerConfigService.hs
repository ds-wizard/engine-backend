module Wizard.Service.Config.ServerConfigService where

import Control.Lens ((^.))
import Data.Yaml (decodeFileEither)

import Shared.Localization.Messages.Internal
import Shared.Model.Error.Error
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigJM ()

import LensesConfig

getServerConfig :: String -> IO (Either AppError ServerConfig)
getServerConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . validateServerConfig $ config
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
    secretLen = length $ config ^. general . secret

validateGeneralServerPort :: ServerConfig -> Either AppError ServerConfig
validateGeneralServerPort config
  | port < 1 || port > 65535 = Left . GeneralServerError $ _ERROR_SERVICE_CONFIG__VALIDATION_SERVER_PORT
  | otherwise = Right config
  where
    port = config ^. general . serverPort
