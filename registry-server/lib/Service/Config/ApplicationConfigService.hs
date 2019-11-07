module Service.Config.ApplicationConfigService where

import Data.Yaml (decodeFileEither)

import Model.Config.AppConfig
import Model.Config.AppConfigJM ()
import Model.Error.Error

getApplicationConfig :: String -> IO (Either AppError AppConfig)
getApplicationConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . Right $ config
    Left error -> return . Left . GeneralServerError . show $ error
