module Wizard.Api.Resource.Project.ProjectSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Project.ProjectSimple

instance FromJSON ProjectSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectSimple where
  toJSON = genericToJSON jsonOptions
