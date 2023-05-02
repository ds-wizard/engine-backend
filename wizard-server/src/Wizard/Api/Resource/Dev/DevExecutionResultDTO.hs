module Wizard.Api.Resource.Dev.DevExecutionResultDTO where

import GHC.Generics

data AdminExecutionResultDTO = AdminExecutionResultDTO
  { output :: String
  }
  deriving (Show, Eq, Generic)
