module Registry.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

instance ToJSON DocumentTemplateFormat where
  toJSON DocumentTemplateFormat {..} =
    object
      [ "uuid" .= uuid
      , "name" .= name
      , "shortName" .= name
      , "icon" .= icon
      , "color" .= "#FFFFFF"
      , "steps" .= steps
      ]

instance ToJSON DocumentTemplateFormatStep where
  toJSON = genericToJSON jsonOptions
