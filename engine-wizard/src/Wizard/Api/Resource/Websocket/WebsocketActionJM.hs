module Wizard.Api.Resource.Websocket.WebsocketActionJM where

import Data.Aeson

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Websocket.WebsocketActionDTO

instance ToJSON a => ToJSON (Success_ServerActionDTO a) where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON a => FromJSON (Success_ServerActionDTO a) where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON Error_ServerActionDTO where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON Error_ServerActionDTO where
  parseJSON = genericParseJSON simpleOptions'''
