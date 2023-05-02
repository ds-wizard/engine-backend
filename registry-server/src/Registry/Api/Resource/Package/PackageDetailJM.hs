module Registry.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageDetailDTO
import Shared.Common.Util.Aeson
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseJM ()

instance FromJSON PackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageDetailDTO where
  toJSON = genericToJSON jsonOptions
