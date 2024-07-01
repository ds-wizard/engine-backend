module WizardLib.Public.Api.Resource.UserToken.UserTokenJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Model.User.UserToken

instance ToJSON UserTokenType

instance ToJSON UserToken where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserTokenDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON UserTokenDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
