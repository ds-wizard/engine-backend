module Wizard.Bootstrap.Messaging where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)

import LensesConfig
import Shared.Bootstrap.Retry
import Shared.Constant.Component
import Shared.Util.Logger
import Wizard.Messaging.Connection

connectMQ serverConfig =
  if serverConfig ^. messaging . enabled
    then do
      logInfo _CMP_MESSAGING "connecting to the message broker"
      msgChannel <-
        liftIO $
        withRetry
          (serverConfig ^. logging)
          retryBackoff
          _CMP_MESSAGING
          "failed to connect to the message broker"
          (createMessagingChannel serverConfig)
      logInfo _CMP_MESSAGING "connected"
      return msgChannel
    else do
      logInfo _CMP_MESSAGING "not enabled - skipping"
      return Nothing
