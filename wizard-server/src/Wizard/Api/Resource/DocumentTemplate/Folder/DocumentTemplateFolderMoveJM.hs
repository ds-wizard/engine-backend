module Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO

instance FromJSON DocumentTemplateFolderMoveDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFolderMoveDTO where
  toJSON = genericToJSON jsonOptions
