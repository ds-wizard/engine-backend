module Messaging.Out.Topic.UserTopic where

import Messaging.Out.Topic.Common
import Messaging.Route
import Model.Context.AppContext
import Model.User.User

publishToUserCreatedTopic :: User -> AppContextM ()
publishToUserCreatedTopic user = do
  let body = "Some body"
  publishMessage _USER_CREATED_TOPIC body
