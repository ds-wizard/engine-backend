module Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM where

import Data.Aeson

import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()

instance FromJSON KnowledgeModelPackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageDetailDTO where
  toJSON = genericToJSON jsonOptions
