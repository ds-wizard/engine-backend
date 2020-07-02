module Wizard.Api.Resource.Template.TemplateSimpleJM where

import Data.Aeson

import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Api.Resource.Template.TemplateStateJM ()

instance FromJSON TemplateSimpleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateSimpleDTO where
  toJSON = genericToJSON simpleOptions
