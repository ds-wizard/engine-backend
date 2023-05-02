module Wizard.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageStateJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseJM ()

instance FromJSON PackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageDetailDTO where
  toJSON = genericToJSON jsonOptions
