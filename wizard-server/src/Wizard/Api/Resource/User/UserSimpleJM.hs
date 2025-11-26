module Wizard.Api.Resource.User.UserSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.User.UserSimple

instance FromJSON UserSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSimple where
  toJSON = genericToJSON jsonOptions
