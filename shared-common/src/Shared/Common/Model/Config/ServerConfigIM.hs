module Shared.Common.Model.Config.ServerConfigIM where

import Control.Monad.Reader (liftIO)
import qualified Crypto.PubKey.RSA as RSA
import qualified Data.ByteString.Char8 as BS
import System.Environment (lookupEnv)
import Text.Read (readEither)

import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.Crypto
import Shared.Common.Util.String

instance FromEnv ServerConfigDatabase where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyStringEnvVariable "DATABASE_CONNECTION_STRING" c.connectionString (\x -> c {connectionString = x})
      , \c -> applyEnvVariable "DATABASE_STRIPE_SIZE" c.stripeSize (\x -> c {stripeSize = x})
      , \c -> applyEnvVariable "DATABASE_CONNECTION_TIMEOUT" c.connectionTimeout (\x -> c {connectionTimeout = x})
      , \c -> applyEnvVariable "DATABASE_MAX_CONNECTIONS" c.maxConnections (\x -> c {maxConnections = x})
      ]

instance FromEnv ServerConfigS3 where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyStringEnvVariable "S3_URL" c.url (\x -> c {url = x} :: ServerConfigS3)
      , \c -> applyStringEnvVariable "S3_USERNAME" c.username (\x -> c {username = x})
      , \c -> applyStringEnvVariable "S3_PASSWORD" c.password (\x -> c {password = x})
      , \c -> applyStringEnvVariable "S3_BUCKET" c.bucket (\x -> c {bucket = x})
      , \c -> applyMaybeStringEnvVariable "S3_REGION" c.region (\x -> c {region = x})
      ]

instance FromEnv ServerConfigAnalytics where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "ANALYTICS_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigAnalytics)
      , \c -> applyStringEnvVariable "ANALYTICS_EMAIL" c.email (\x -> c {email = x})
      ]

instance FromEnv ServerConfigSentry where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "SENTRY_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigSentry)
      , \c -> applyStringEnvVariable "SENTRY_DSN" c.dsn (\x -> c {dsn = x})
      ]

instance FromEnv ServerConfigPersistentCommand where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "PERSISTENT_COMMAND_LISTENER_JOB_ENABLED" c.listenerJob.enabled (\x -> c {listenerJob = c.listenerJob {enabled = x}} :: ServerConfigPersistentCommand)
      , \c -> applyEnvVariable "PERSISTENT_COMMAND_RETRY_JOB_ENABLED" c.retryJob.enabled (\x -> c {retryJob = c.retryJob {enabled = x}} :: ServerConfigPersistentCommand)
      , \c -> applyStringEnvVariable "PERSISTENT_COMMAND_RETRY_JOB_CRON" c.retryJob.cron (\x -> c {retryJob = c.retryJob {cron = x}} :: ServerConfigPersistentCommand)
      ]

instance FromEnv ServerConfigLogging where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "LOGGING_LEVEL" c.level (\x -> c {level = x})
      , \c -> applyEnvVariable "LOGGING_HTTP_CLIENT_DEBUG" c.httpClientDebug (\x -> c {httpClientDebug = x})
      , \c -> applyEnvVariable "LOGGING_WEBSOCKET_DEBUG" c.websocketDebug (\x -> c {websocketDebug = x})
      ]

instance FromEnv ServerConfigCloud where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "CLOUD_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigCloud)
      , \c -> applyMaybeStringEnvVariable "CLOUD_DOMAIN" c.domain (\x -> c {domain = x})
      , \c -> applyEnvVariable "CLOUD_PUBLIC_REGISTRATION_ENABLED" c.publicRegistrationEnabled (\x -> c {publicRegistrationEnabled = x})
      ]

-- --------------------------------------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------------------------------------
class FromEnv config where
  applyEnv :: config -> IO config

applyEnvVariables :: serverConfig -> [serverConfig -> IO serverConfig] -> IO serverConfig
applyEnvVariables serverConfig =
  foldl
    ( \scIO fn -> do
        sc <- scIO
        fn sc
    )
    (return serverConfig)

applyStringEnvVariable :: String -> String -> (String -> serverConfig) -> IO serverConfig
applyStringEnvVariable envVariableName oldValue updateFn = do
  envVariable <- liftIO $ lookupEnv envVariableName
  case envVariable of
    Just envVariable -> do
      print $ f' "Config: Applying env variable %s" [envVariableName]
      return . updateFn $ envVariable
    Nothing -> return $ updateFn oldValue

applyEnvVariable :: Read value => String -> value -> (value -> serverConfig) -> IO serverConfig
applyEnvVariable envVariableName oldValue updateFn = do
  envVariable <- liftIO $ lookupEnv envVariableName
  case envVariable of
    Just envVariableS ->
      case readEither envVariableS of
        Right envVariable -> do
          print $ f' "Config: Applying env variable %s" [envVariableName]
          return . updateFn $ envVariable
        Left error -> do
          print $ f' "Config: Failed to decode env variable '%s' with the value '%s'" [envVariableName, envVariableS]
          return $ updateFn oldValue
    Nothing -> return $ updateFn oldValue

applyMaybeStringEnvVariable :: String -> Maybe String -> (Maybe String -> serverConfig) -> IO serverConfig
applyMaybeStringEnvVariable envVariableName oldValue updateFn = do
  envVariable <- liftIO $ lookupEnv envVariableName
  case envVariable of
    Just envVariable -> do
      print $ f' "Config: Applying env variable %s" [envVariableName]
      return . updateFn . Just $ envVariable
    Nothing -> return $ updateFn oldValue

applyRSAPrivateKeyEnvVariable :: String -> RSA.PrivateKey -> (RSA.PrivateKey -> serverConfig) -> IO serverConfig
applyRSAPrivateKeyEnvVariable envVariableName oldValue updateFn = do
  envVariable <- liftIO $ lookupEnv envVariableName
  case envVariable of
    Just envVariable -> do
      case readRSAPrivateKey . BS.pack $ envVariable of
        Just privateKey -> do
          print $ f' "Config: Applying env variable %s" [envVariableName]
          return . updateFn $ privateKey
        Nothing -> do
          print $ f' "Config: Failed to decode env variable '%s' with the value '%s'" [envVariableName, envVariable]
          print _ERROR_SERVICE_CONFIG__VALIDATION_ENV_RSA_PRIVATE_KEY_FORMAT
          return $ updateFn oldValue
    Nothing -> return $ updateFn oldValue
