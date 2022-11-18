module Wizard.Api.Resource.Dev.DevExecutionDTO where

import GHC.Generics

data DevExecutionDTO = DevExecutionDTO
  { sectionName :: String
  , operationName :: String
  , parameters :: [String]
  }
  deriving (Show, Eq, Generic)
