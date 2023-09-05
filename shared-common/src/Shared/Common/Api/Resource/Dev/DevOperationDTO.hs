module Shared.Common.Api.Resource.Dev.DevOperationDTO where

import GHC.Generics

import Shared.Common.Model.Dev.Dev

data DevOperationDTO = DevOperationDTO
  { name :: String
  , description :: Maybe String
  , parameters :: [DevOperationParameter]
  }
  deriving (Show, Eq, Generic)
