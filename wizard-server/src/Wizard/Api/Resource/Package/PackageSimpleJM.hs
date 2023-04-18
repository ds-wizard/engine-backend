module Wizard.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageStateJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationJM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseJM ()
import WizardLib.KnowledgeModel.Model.Package.PackageSimple

instance FromJSON PackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageSimpleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON PackageSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageSimple where
  toJSON = genericToJSON jsonOptions
