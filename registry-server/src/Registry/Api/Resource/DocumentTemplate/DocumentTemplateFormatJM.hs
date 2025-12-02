module Registry.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO

instance ToJSON DocumentTemplateFormatDTO where
  toJSON DocumentTemplateFormatDTO {..} =
    object
      [ "uuid" .= uuid
      , "name" .= name
      , "shortName" .= name
      , "icon" .= icon
      , "color" .= "#FFFFFF"
      , "steps" .= steps
      ]

instance ToJSON DocumentTemplateFormatStepDTO where
  toJSON = genericToJSON jsonOptions
