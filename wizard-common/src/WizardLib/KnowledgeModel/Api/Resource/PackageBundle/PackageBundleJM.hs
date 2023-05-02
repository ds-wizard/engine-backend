module WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageJM ()
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO

instance FromJSON PackageBundleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageBundleDTO where
  toJSON = genericToJSON jsonOptions
