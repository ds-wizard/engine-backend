module Wizard.Api.Resource.Auth.AuthConsentJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Auth.AuthConsentDTO

instance FromJSON AuthConsentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AuthConsentDTO where
  toJSON = genericToJSON jsonOptions
