module Wizard.Api.Resource.Websocket.BranchActionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Branch.Event.BranchEventJM ()
import Wizard.Api.Resource.User.OnlineUserInfoJM ()
import Wizard.Api.Resource.Websocket.BranchActionDTO

instance FromJSON ClientBranchActionDTO where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON ClientBranchActionDTO where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON ServerBranchActionDTO where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON ServerBranchActionDTO where
  toJSON = genericToJSON simpleOptions'''
