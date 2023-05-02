module Wizard.Api.Resource.Websocket.WebsocketActionJM where

import Data.Aeson

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Websocket.WebsocketActionDTO

instance ToJSON a => ToJSON (Success_ServerActionDTO a) where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON a => FromJSON (Success_ServerActionDTO a) where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON Error_ServerActionDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON Error_ServerActionDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")
