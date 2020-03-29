module Wizard.Service.Config.ServerConfigService where

import Data.Yaml (decodeFileEither)

import Shared.Model.Error.Error
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigJM ()

getServerConfig :: String -> IO (Either AppError ServerConfig)
getServerConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . Right $ config
    Left error -> return . Left . GeneralServerError . show $ error
