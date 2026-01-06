module Wizard.Api.Resource.Document.DocumentJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Project.ProjectSimpleJM ()
import Wizard.Model.Document.Document

instance FromJSON DocumentState

instance ToJSON DocumentState

instance FromJSON DocumentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentDTO where
  toJSON = genericToJSON jsonOptions
