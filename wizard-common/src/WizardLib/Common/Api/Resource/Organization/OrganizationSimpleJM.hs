module WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO

instance FromJSON OrganizationSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OrganizationSimpleDTO where
  toJSON = genericToJSON jsonOptions
