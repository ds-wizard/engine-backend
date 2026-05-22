module Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO

instance FromJSON aType => FromJSON (UserEmailLinkDTO aType) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON aType => ToJSON (UserEmailLinkDTO aType) where
  toJSON = genericToJSON jsonOptions
