module Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateFolderMoveDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFolderMoveDTO where
  toJSON = genericToJSON jsonOptions
