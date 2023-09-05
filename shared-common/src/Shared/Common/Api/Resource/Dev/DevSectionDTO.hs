module Shared.Common.Api.Resource.Dev.DevSectionDTO where

import GHC.Generics

import Shared.Common.Api.Resource.Dev.DevOperationDTO

data DevSectionDTO = DevSectionDTO
  { name :: String
  , description :: Maybe String
  , operations :: [DevOperationDTO]
  }
  deriving (Show, Eq, Generic)
