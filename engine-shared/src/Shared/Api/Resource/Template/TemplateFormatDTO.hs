module Shared.Api.Resource.Template.TemplateFormatDTO where

import qualified Data.UUID as U
import GHC.Generics

data TemplateFormatDTO = TemplateFormatDTO
  { uuid :: U.UUID
  , name :: String
  , shortName :: String
  , icon :: String
  , color :: String
  , isPdf :: Bool
  }
  deriving (Show, Eq, Generic)
