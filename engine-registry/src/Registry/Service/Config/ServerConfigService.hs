module Registry.Service.Config.ServerConfigService where

import Data.Yaml (decodeFileEither)

import Registry.Model.Config.ServerConfig
import Registry.Model.Config.ServerConfigJM ()
import Shared.Model.Error.Error

getServerConfig :: String -> IO (Either AppError ServerConfig)
getServerConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . Right $ config
    Left error -> return . Left . GeneralServerError . show $ error
