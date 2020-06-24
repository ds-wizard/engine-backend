module Wizard.Messaging.Out.Queue.Common
  ( createQueue
  , createBody
  , publishToQueue
  ) where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Text as T
import Network.AMQP

import Shared.Constant.Component
import Wizard.Model.Context.AppContext
import Wizard.Util.Logger

createQueue name = newQueue {queueName = name}

createBody body = newMsg {msgBody = body}

publishToQueue queue body = do
  msgChannel <- asks _appContextMsgChannel
  publishMessageToQueue msgChannel queue body

-- --------------------------------
-- PRIVATE
-- --------------------------------
publishMessageToQueue Nothing _ _ = return ()
publishMessageToQueue (Just msgChannel) queueName body = do
  let queueNameText = T.pack queueName
  liftIO $ declareQueue msgChannel (createQueue queueNameText)
  liftIO $ publishMsg msgChannel "" queueNameText (createBody body)
  logInfoU _CMP_MESSAGING ("PublishToQueue: " ++ queueName)
  return ()
