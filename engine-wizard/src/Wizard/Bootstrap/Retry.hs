module Wizard.Bootstrap.Retry where

import Control.Lens ((^.))
import Control.Monad.Catch
import Control.Retry

import LensesConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Util.Logger

retryCount = 5

retryBaseWait = 2000000

retryBackoff :: RetryPolicyM IO
retryBackoff = exponentialBackoff retryBaseWait <> limitRetries retryCount

withRetry :: ServerConfig -> RetryPolicyM IO -> String -> String -> IO a -> IO a
withRetry serverConfig backoff _CMP description action = recovering backoff handlers wrappedAction
  where
    wrappedAction _ = action
    handlers = skipAsyncExceptions ++ [handler]
    handler retryStatus = Handler $ \(_ :: SomeException) -> loggingHandler retryStatus
    loggingLevel = serverConfig ^. logging . level
    loggingHandler retryStatus = do
      let nextWait =
            case rsPreviousDelay retryStatus of
              Just x -> 2 * fromIntegral x / 1000000
              Nothing -> fromIntegral retryBaseWait / 1000000
      if rsIterNumber retryStatus < retryCount
        then do
          let retryInfo = "retry #" ++ show (rsIterNumber retryStatus + 1) ++ " in " ++ show nextWait ++ " seconds"
          runLogging loggingLevel $ logWarn _CMP (description ++ " - " ++ retryInfo)
        else runLogging loggingLevel $ logError _CMP description
      return True
