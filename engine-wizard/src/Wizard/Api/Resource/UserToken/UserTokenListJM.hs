module Wizard.Api.Resource.UserToken.UserTokenListJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.User.UserTokenList

instance FromJSON UserTokenList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserTokenList where
  toJSON = genericToJSON jsonOptions
