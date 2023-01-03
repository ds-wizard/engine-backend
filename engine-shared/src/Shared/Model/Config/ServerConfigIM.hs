module Shared.Model.Config.ServerConfigIM where

import Control.Monad.Reader (liftIO)
import System.Environment (lookupEnv)
import Text.Read (readEither)

import Shared.Model.Config.ServerConfig
import Shared.Util.String

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
      , \c -> applyStringEnvVariable "S3_PUBLIC_URL" c.publicUrl (\x -> c {publicUrl = x})
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
