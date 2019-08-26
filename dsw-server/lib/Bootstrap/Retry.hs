module Bootstrap.Retry where

import Control.Monad.Catch
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Retry

import Util.Logger

retryCount = 5

retryBaseWait = 2000000

retryBackoff :: RetryPolicyM IO
retryBackoff = exponentialBackoff retryBaseWait <> limitRetries retryCount

withRetry :: RetryPolicyM IO -> String -> String -> IO a -> IO a
withRetry backoff _CMP description action = recovering backoff handlers wrappedAction
  where
    wrappedAction _ = action
    handlers = skipAsyncExceptions ++ [handler]
    handler retryStatus = Handler $ \(_ :: SomeException) -> loggingHandler retryStatus
    loggingHandler retryStatus = do
      let nextWait =
            case rsPreviousDelay retryStatus of
              Just x -> 2 * (fromIntegral x) / 1000000
              Nothing -> fromIntegral retryBaseWait / 1000000
      if rsIterNumber retryStatus < retryCount
        then do
          let retryInfo = "retry #" ++ show (rsIterNumber retryStatus + 1) ++ " in " ++ show nextWait ++ " seconds"
          runStdoutLoggingT $ logWarn $ msg _CMP (description ++ " - " ++ retryInfo)
        else runStdoutLoggingT $ logError $ msg _CMP description
      return True
