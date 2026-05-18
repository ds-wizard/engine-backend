module Shared.Common.Bootstrap.AwsAppConfig (
  resolveConfigPath,
  resolveConfigBytes,
) where

import qualified Amazonka as AWS
import Amazonka.AppConfigData
import Amazonka.AppConfigData.GetLatestConfiguration
import Amazonka.AppConfigData.StartConfigurationSession
import Control.Concurrent (MVar, threadDelay, tryPutMVar)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Shared.Common.Util.String
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hClose, openTempFile)

import Shared.Common.Integration.Aws.Common

resolveConfigPath :: FilePath -> IO (FilePath, MVar () -> IO ())
resolveConfigPath defaultPath = do
  mResult <- fetchFromAppConfig defaultPath
  case mResult of
    Just (yamlBytes, profileIdentifier, poller) -> do
      path <- writeTempYaml profileIdentifier yamlBytes
      logInfo $ f' "configuration written to temp file: %s" [path]
      return (path, poller)
    Nothing -> do
      logInfo $ f' "using local config file: %s" [defaultPath]
      return (defaultPath, \_ -> return ())

resolveConfigBytes :: FilePath -> IO (ByteString, MVar () -> IO ())
resolveConfigBytes defaultPath = do
  mResult <- fetchFromAppConfig defaultPath
  case mResult of
    Just (yamlBytes, _, poller) -> do
      logInfo "configuration loaded into memory (no temp file written)"
      return (yamlBytes, poller)
    Nothing -> do
      mOverride <- lookupEnv "APPLICATION_CONFIG_PATH"
      let path = fromMaybe defaultPath mOverride
      when (isJust mOverride) $ logInfo $ f' "overriding the config path with '%s'" [path]
      logInfo $ f' "using local config file: %s" [path]
      bs <- BS.readFile path
      return (bs, \_ -> return ())

-- ---------------------------------------------------------------------------
-- PRIVATE
-- ---------------------------------------------------------------------------
fetchFromAppConfig :: FilePath -> IO (Maybe (ByteString, String, MVar () -> IO ()))
fetchFromAppConfig defaultPath = do
  mAppId <- lookupEnv "AWS_APP_CONFIG"
  mAccessKeyId <- lookupEnv "AWS_ACCESS_KEY_ID"
  mSecretAccessKey <- lookupEnv "AWS_SECRET_ACCESS_KEY"
  mRegion <- lookupEnv "AWS_REGION"
  case (mAppId, mAccessKeyId, mSecretAccessKey, mRegion) of
    (Just appId, Just accessKeyId, Just secretAccessKey, Just region) -> do
      let envIdentifier = "Default"
      let profileIdentifier = replace "." "-" . last . splitOn "/" $ defaultPath
      debug <- isDebugEnabled
      logInfo $
        f'
          "loading from AWS AppConfig (app=%s, env=%s, profile=%s, debug=%s)"
          [appId, envIdentifier, profileIdentifier, show debug]
      logInfo "starting AppConfig configuration session"
      (yamlBytes, nextToken, intervalSec) <- fetchInitialConfig accessKeyId secretAccessKey region appId envIdentifier profileIdentifier
      logInfo $
        f'
          "initial configuration loaded (%s bytes); next poll in %ss; next token=%s"
          [show (BS.length yamlBytes), show intervalSec, take 8 (T.unpack nextToken)]
      return $ Just (yamlBytes, profileIdentifier, pollLoop debug accessKeyId secretAccessKey region nextToken intervalSec)
    _ -> return Nothing

fetchInitialConfig :: String -> String -> String -> String -> String -> String -> IO (ByteString, Text, Int)
fetchInitialConfig accessKeyId secretAccessKey region appId envId profileId =
  runAwsRequest accessKeyId secretAccessKey region $ \env -> do
    sessionResp <- AWS.send env $ newStartConfigurationSession (T.pack appId) (T.pack envId) (T.pack profileId)
    token <- case sessionResp.initialConfigurationToken of
      Just t -> return t
      Nothing -> liftIO $ die "AppConfig StartConfigurationSession did not return an initial token"
    configResp <- AWS.send env $ newGetLatestConfiguration token
    bs <- case configResp.configuration of
      Just (AWS.Sensitive b) -> return b
      Nothing -> liftIO $ die "AppConfig GetLatestConfiguration returned empty configuration body"
    nextToken <- case configResp.nextPollConfigurationToken of
      Just t -> return t
      Nothing -> liftIO $ die "AppConfig GetLatestConfiguration did not return a next-poll token"
    let interval = fromMaybe 60 configResp.nextPollIntervalInSeconds
    return (bs, nextToken, interval)

pollLoop :: Bool -> String -> String -> String -> Text -> Int -> MVar () -> IO ()
pollLoop debug accessKeyId secretAccessKey region initialToken initialInterval shutdownFlag = do
  logInfo $ f' "starting poll loop (interval=%ss)" [show initialInterval]
  loop initialToken initialInterval (0 :: Int)
  where
    loop token interval iter = do
      threadDelay (interval * 1000000)
      let iter' = iter + 1
      logDebug debug $ f' "poll #%s (token=%s)" [show iter', take 8 (T.unpack token)]
      result <-
        try $
          runAwsRequest accessKeyId secretAccessKey region $ \env ->
            AWS.send env $ newGetLatestConfiguration token
      case result of
        Left (e :: SomeException) -> do
          logInfo $ f' "poll #%s failed: %s — retrying after %ss" [show iter', show e, show interval]
          loop token interval iter'
        Right configResp -> do
          let nextToken = fromMaybe token configResp.nextPollConfigurationToken
              nextInterval = fromMaybe interval configResp.nextPollIntervalInSeconds
          case configResp.configuration of
            Just (AWS.Sensitive bs) | not (BS.null bs) -> do
              logInfo $
                f'
                  "configuration changed at poll #%s (%s bytes) — triggering shutdown for restart"
                  [show iter', show (BS.length bs)]
              _ <- tryPutMVar shutdownFlag ()
              return ()
            _ -> do
              logDebug debug $
                f'
                  "poll #%s no change; next poll in %ss; next token=%s"
                  [show iter', show nextInterval, take 8 (T.unpack nextToken)]
              loop nextToken nextInterval iter'

-- ---------------------------------------------------------------------------
-- HELPERS
-- ---------------------------------------------------------------------------
writeTempYaml :: String -> ByteString -> IO FilePath
writeTempYaml profileIdentifier bs = do
  (path, h) <- openTempFile "/tmp" (f' "%s-.yml" [profileIdentifier])
  BS.hPut h bs
  hClose h
  return path

isDebugEnabled :: IO Bool
isDebugEnabled = do
  m <- lookupEnv "AWS_APP_CONFIG_DEBUG"
  return $ case m of
    Just s | not (null s) -> True
    _ -> False

logInfo :: String -> IO ()
logInfo msg = putStrLn $ f' "CONFIG: %s" [msg]

logDebug :: Bool -> String -> IO ()
logDebug debug msg = when debug $ putStrLn $ f' "CONFIG [debug]: %s" [msg]
