module Wizard.Api.Resource.Project.Acl.ProjectPermJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Acl.MemberJM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.Acl.ProjectPerm

instance FromJSON ProjectPermType

instance ToJSON ProjectPermType

instance FromJSON ProjectPerm where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectPerm where
  toJSON = genericToJSON jsonOptions

instance FromJSON ProjectPermDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectPermDTO where
  toJSON = genericToJSON jsonOptions
