module Wizard.Api.Resource.UserToken.UserTokenListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.User.UserTokenList

instance FromJSON UserTokenList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserTokenList where
  toJSON = genericToJSON jsonOptions
