module Registry.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageDetailDTO
import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseJM ()

instance FromJSON PackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageDetailDTO where
  toJSON = genericToJSON jsonOptions
