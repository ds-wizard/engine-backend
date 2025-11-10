module RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleJM where

import Data.Aeson

import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import Shared.Common.Util.Aeson

instance FromJSON KnowledgeModelPackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSimpleDTO where
  toJSON = genericToJSON jsonOptions
