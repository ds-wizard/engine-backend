module Wizard.Api.Resource.Websocket.BranchActionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Branch.Event.BranchEventJM ()
import Wizard.Api.Resource.Branch.Event.SetRepliesJM ()
import Wizard.Api.Resource.User.OnlineUserInfoJM ()
import Wizard.Api.Resource.Websocket.BranchActionDTO

instance FromJSON ClientBranchActionDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ClientBranchActionDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ServerBranchActionDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ServerBranchActionDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
