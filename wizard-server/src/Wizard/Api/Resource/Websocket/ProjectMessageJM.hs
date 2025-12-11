module Wizard.Api.Resource.Websocket.ProjectMessageJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Detail.ProjectDetailWsJM ()
import Wizard.Api.Resource.Project.Event.ProjectEventChangeJM ()
import Wizard.Api.Resource.Project.Event.ProjectEventJM ()
import Wizard.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Wizard.Api.Resource.Project.ProjectReplyJM ()
import Wizard.Api.Resource.User.OnlineUserInfoJM ()
import Wizard.Api.Resource.Websocket.ProjectMessageDTO

instance FromJSON ClientProjectMessageDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ClientProjectMessageDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ServerProjectMessageDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ServerProjectMessageDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
