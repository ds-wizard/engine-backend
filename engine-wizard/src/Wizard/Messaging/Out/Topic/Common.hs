module Wizard.Messaging.Out.Topic.Common
  ( createTopic
  , createTopicMessage
  , publishMessage
  ) where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Text as T
import Network.AMQP

import Wizard.Constant.Component
import Wizard.Model.Context.AppContext
import Wizard.Util.Logger

createTopic name = newExchange {exchangeName = name, exchangeType = "topic", exchangeDurable = False}

createTopicMessage body = newMsg {msgBody = body, msgDeliveryMode = Just NonPersistent}

publishMessage topicName body = do
  msgChannel <- asks _appContextMsgChannel
  publishMessageOnChannel msgChannel topicName body

-- --------------------------------
-- PRIVATE
-- --------------------------------
publishMessageOnChannel Nothing _ _ = return ()
publishMessageOnChannel (Just msgChannel) topicName body = do
  let topicNameText = T.pack topicName
  liftIO $ declareExchange msgChannel (createTopic topicNameText)
  liftIO $ publishMsg msgChannel topicNameText "" (createTopicMessage body)
  logInfoU $ msg _CMP_MESSAGING ("PublishTopic: " ++ topicName)
  return ()
