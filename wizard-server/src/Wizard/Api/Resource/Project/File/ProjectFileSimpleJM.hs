module Wizard.Api.Resource.Project.File.ProjectFileSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Project.File.ProjectFileSimple

instance FromJSON ProjectFileSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectFileSimple where
  toJSON = genericToJSON jsonOptions
