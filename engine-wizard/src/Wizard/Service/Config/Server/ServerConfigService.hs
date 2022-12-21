module Wizard.Service.Config.Server.ServerConfigService where

import Control.Monad.Reader (liftIO)
import Data.Yaml (decodeFileEither)
import System.Environment (lookupEnv)

import Shared.Localization.Messages.Internal
import Shared.Model.Config.ServerConfig
import Shared.Model.Error.Error
import Shared.Util.String
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigJM ()

getServerConfig :: String -> IO (Either AppError ServerConfig)
getServerConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> do
      updatedConfig <- applyEnvVariables config
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

applyEnvVariables :: ServerConfig -> IO ServerConfig
applyEnvVariables serverConfig =
  foldl
    ( \scIO fn -> do
        sc <- scIO
        fn sc
    )
    (return serverConfig)
    envs

applyEnvVariable :: String -> String -> (String -> ServerConfig) -> IO ServerConfig
applyEnvVariable envVariableName oldValue updateFn = do
  envVariable <- liftIO $ lookupEnv envVariableName
  case envVariable of
    Just envVariable -> do
      print $ f' "Config: Applying env variable %s" [envVariableName]
      return . updateFn $ envVariable
    Nothing -> return $ updateFn oldValue

applyMaybeEnvVariable :: String -> Maybe String -> (Maybe String -> ServerConfig) -> IO ServerConfig
applyMaybeEnvVariable envVariableName oldValue updateFn = do
  envVariable <- liftIO $ lookupEnv envVariableName
  case envVariable of
    Just envVariable -> do
      print $ f' "Config: Applying env variable %s" [envVariableName]
      return . updateFn . Just $ envVariable
    Nothing -> return $ updateFn oldValue

envs :: [ServerConfig -> IO ServerConfig]
envs =
  [ \c -> applyEnvVariable "GENERAL_SECRET" c.general.secret (\x -> c {general = c.general {secret = x}})
  , \c -> applyEnvVariable "DATABASE_CONNECTION_STRING" c.database.connectionString (\x -> c {database = c.database {connectionString = x}})
  , \c -> applyEnvVariable "S3_URL" c.s3.url (\x -> c {s3 = c.s3 {url = x}})
  , \c -> applyEnvVariable "S3_PUBLIC_URL" c.s3.publicUrl (\x -> c {s3 = c.s3 {publicUrl = x}})
  , \c -> applyEnvVariable "S3_USERNAME" c.s3.username (\x -> c {s3 = c.s3 {username = x}})
  , \c -> applyEnvVariable "S3_PASSWORD" c.s3.password (\x -> c {s3 = c.s3 {password = x}})
  , \c -> applyEnvVariable "S3_BUCKET" c.s3.bucket (\x -> c {s3 = c.s3 {bucket = x}})
  , \c -> applyMaybeEnvVariable "S3_REGION" c.s3.region (\x -> c {s3 = c.s3 {region = x}})
  ]
