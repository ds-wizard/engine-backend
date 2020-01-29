module Wizard.Messaging.Out.Topic.User where

import Wizard.Messaging.Out.Topic.Common
import Wizard.Messaging.Route
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

publishToUserCreatedTopic :: User -> AppContextM ()
publishToUserCreatedTopic user = do
  let body = "Some body"
  publishMessage _USER_CREATED_TOPIC body
