module WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

instance FromJSON PackagePattern where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackagePattern where
  toJSON = genericToJSON jsonOptions
