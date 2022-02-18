module Shared.Api.Resource.Template.TemplateFormatDTO where

import qualified Data.UUID as U
import GHC.Generics

data TemplateFormatDTO =
  TemplateFormatDTO
    { _templateFormatDTOUuid :: U.UUID
    , _templateFormatDTOName :: String
    , _templateFormatDTOShortName :: String
    , _templateFormatDTOIcon :: String
    , _templateFormatDTOColor :: String
    , _templateFormatDTOIsPdf :: Bool
    }
  deriving (Show, Eq, Generic)
