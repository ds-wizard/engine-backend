module Service.Config.BuildInfoConfigService where

import Data.Yaml (decodeFileEither)

import Model.Config.BuildInfoConfig
import Model.Config.BuildInfoConfigJM ()
import Model.Error.Error

getBuildInfoConfig :: String -> IO (Either AppError BuildInfoConfig)
getBuildInfoConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . Right $ config
    Left error -> return . Left . GeneralServerError . show $ error
