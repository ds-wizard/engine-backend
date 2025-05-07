module WizardLib.Public.Api.Resource.User.UserLocaleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.User.UserLocaleDTO

instance FromJSON UserLocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserLocaleDTO where
  toJSON = genericToJSON jsonOptions
