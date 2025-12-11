module Wizard.Api.Resource.Project.ProjectContentChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Event.ProjectEventChangeJM ()
import Wizard.Api.Resource.Project.ProjectContentChangeDTO

instance FromJSON ProjectContentChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectContentChangeDTO where
  toJSON = genericToJSON jsonOptions
