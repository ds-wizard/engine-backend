module Shared.Common.Service.Config.Server.ServerConfigService where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Yaml (decodeEither')

import Shared.Common.Model.Config.ServerConfigIM
import Shared.Common.Model.Error.Error

getServerConfig :: (FromJSON serverConfig, FromEnv serverConfig) => (serverConfig -> Either AppError serverConfig) -> ByteString -> IO (Either AppError serverConfig)
getServerConfig validateServerConfig bs =
  case decodeEither' bs of
    Right config -> do
      updatedConfig <- applyEnv config
      return . validateServerConfig $ updatedConfig
    Left error -> return . Left . GeneralServerError . show $ error
