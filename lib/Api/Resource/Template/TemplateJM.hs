module Api.Resource.Template.TemplateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Template.TemplateDTO

instance FromJSON TemplateDTO where
  parseJSON (Object o) = do
    _templateDTOUuid <- o .: "uuid"
    _templateDTOName <- o .: "name"
    _templateDTORootFile <- o .: "rootFile"
    return TemplateDTO {..}
  parseJSON _ = mzero

instance ToJSON TemplateDTO where
  toJSON TemplateDTO {..} =
    object ["uuid" .= _templateDTOUuid, "name" .= _templateDTOName, "rootFile" .= _templateDTORootFile]
