module Wizard.Service.Config.BuildInfoConfigService where

import Data.Yaml (decodeFileEither)

import Shared.Model.Error.Error
import Wizard.Model.Config.BuildInfoConfig
import Wizard.Model.Config.BuildInfoConfigJM ()

getBuildInfoConfig :: String -> IO (Either AppError BuildInfoConfig)
getBuildInfoConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return . Right $ config
    Left error -> return . Left . GeneralServerError . show $ error
