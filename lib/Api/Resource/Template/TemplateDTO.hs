module Api.Resource.Template.TemplateDTO where

import qualified Data.UUID as U

data TemplateDTO = TemplateDTO
  { _templateDTOUuid :: U.UUID
  , _templateDTOName :: String
  , _templateDTORootFile :: String
  } deriving (Show)
