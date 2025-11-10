module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM where

import Data.Aeson

import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()

instance FromJSON DocumentTemplateSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimpleDTO where
  toJSON = genericToJSON jsonOptions
