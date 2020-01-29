module Wizard.Bootstrap.Messaging where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)

import LensesConfig
import Wizard.Bootstrap.Retry
import Wizard.Constant.Component
import Wizard.Messaging.Connection
import Wizard.Util.Logger

connectMQ appConfig =
  if appConfig ^. messaging . enabled
    then do
      logInfo $ msg _CMP_MESSAGING "connecting to the message broker"
      msgChannel <-
        liftIO $
        withRetry
          retryBackoff
          _CMP_MESSAGING
          "failed to connect to the message broker"
          (createMessagingChannel appConfig)
      logInfo $ msg _CMP_MESSAGING "connected"
      return msgChannel
    else do
      logInfo $ msg _CMP_MESSAGING "not enabled - skipping"
      return Nothing
