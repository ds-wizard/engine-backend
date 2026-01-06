module Wizard.Api.Resource.Project.Action.ProjectActionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON ProjectActionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectActionDTO where
  toJSON = genericToJSON jsonOptions
