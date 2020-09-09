module Wizard.Messaging.In.Queue.Common
  ( consumeQueue
  ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson
import qualified Data.Text as T
import Network.AMQP
import Prelude hiding (log)

import LensesConfig
import Shared.Constant.Component
import Wizard.Model.Context.AppContext
import Wizard.Util.Context
import Wizard.Util.Logger

createQueue name = newQueue {queueName = name}

consumeQueue :: FromJSON reqDto => String -> (reqDto -> AppContextM a) -> AppContextM (Maybe ConsumerTag)
consumeQueue queueName handler = do
  context <- ask
  let channel = context ^. msgChannel
  consumeQueueFromQueue channel queueName (runConsumeMessage queueName handler context)

-- --------------------------------
-- PRIVATE
-- --------------------------------
consumeQueueFromQueue Nothing _ _ = return Nothing
consumeQueueFromQueue (Just msgChannel) queueName handler = do
  let queueNameText = T.pack queueName
  liftIO $ declareQueue msgChannel (createQueue queueNameText)
  consumerTag <- liftIO $ consumeMsgs msgChannel queueNameText Ack handler
  log queueName "Start consuming queue"
  return . Just $ consumerTag

runConsumeMessage ::
     FromJSON reqDto => String -> (reqDto -> AppContextM a) -> AppContext -> (Message, Envelope) -> IO ()
runConsumeMessage queueName handler context (msg, env) = void $ runAppContextWithAppContext go context
  where
    go :: AppContextM ()
    go = do
      log queueName "New message recieved"
      case eitherDecode $ msgBody msg of
        Right event -> do
          log queueName "Message successfully deserialized"
          handler event
          liftIO $ ackEnv env
          log queueName "Message successfully processed"
          return ()
        Left error -> do
          log queueName "Message deserialization failed"
          return ()

log queueName msg = logInfoU _CMP_MESSAGING (format "[Q:%s] %s" [queueName, msg])
