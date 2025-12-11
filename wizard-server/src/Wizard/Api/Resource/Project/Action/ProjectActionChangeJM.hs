module Wizard.Api.Resource.Project.Action.ProjectActionChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Action.ProjectActionChangeDTO

instance FromJSON ProjectActionChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectActionChangeDTO where
  toJSON = genericToJSON jsonOptions
