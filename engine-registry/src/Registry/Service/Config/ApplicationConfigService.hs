module Registry.Service.Config.ApplicationConfigService where

import Data.Yaml (decodeFileEither)

import Registry.Model.Config.AppConfig
import Registry.Model.Config.AppConfigJM ()
import Shared.Model.Error.Error

getApplicationConfig :: String -> IO (Either AppError AppConfig)
getApplicationConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . Right $ config
    Left error -> return . Left . GeneralServerError . show $ error
