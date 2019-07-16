module Api.Resource.Template.TemplateDTO where

import qualified Data.UUID as U
import GHC.Generics

data TemplateDTO = TemplateDTO
  { _templateDTOUuid :: U.UUID
  , _templateDTOName :: String
  , _templateDTORootFile :: String
  } deriving (Show, Generic)
