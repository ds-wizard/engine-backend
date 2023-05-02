module Wizard.Api.Resource.Package.PackageChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Package.PackageChangeDTO
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseJM ()

instance FromJSON PackageChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageChangeDTO where
  toJSON = genericToJSON jsonOptions
