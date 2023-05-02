module Registry.Api.Resource.Package.PackageRawJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageRawDTO
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseJM ()

instance ToJSON PackageRawDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON PackageRawDTO where
  parseJSON = genericParseJSON jsonOptions
