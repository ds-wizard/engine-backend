module Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO

instance FromJSON DocumentTemplateFolderDeleteDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFolderDeleteDTO where
  toJSON = genericToJSON jsonOptions
