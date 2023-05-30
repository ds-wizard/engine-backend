module Shared.Common.Service.Config.Server.ServerConfigService where

import Control.Monad.Reader (liftIO, when)
import Data.Aeson
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Yaml (decodeFileEither)
import System.Environment (lookupEnv)

import Shared.Common.Model.Config.ServerConfigIM
import Shared.Common.Model.Error.Error

getServerConfig :: (FromJSON serverConfig, FromEnv serverConfig) => (serverConfig -> Either AppError serverConfig) -> String -> IO (Either AppError serverConfig)
getServerConfig validateServerConfig fileNameBase = do
  mFileNameEnv <- liftIO $ lookupEnv "APPLICATION_CONFIG_PATH"
  when (isJust mFileNameEnv) (putStrLn $ "CONFIG: overriding the config path with '" ++ fromJust mFileNameEnv ++ "'")
  let fileName = fromMaybe fileNameBase mFileNameEnv
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> do
      updatedConfig <- applyEnv config
      return . validateServerConfig $ updatedConfig
    Left error -> return . Left . GeneralServerError . show $ error
