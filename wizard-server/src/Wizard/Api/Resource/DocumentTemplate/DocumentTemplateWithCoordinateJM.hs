module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.DocumentTemplate.DocumentTemplateWithCoordinate

instance FromJSON DocumentTemplateWithCoordinate where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateWithCoordinate where
  toJSON = genericToJSON jsonOptions
