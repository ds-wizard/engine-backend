module Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Common.Util.Aeson

instance FromJSON DocumentTemplateDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDetailDTO where
  toJSON = genericToJSON jsonOptions
