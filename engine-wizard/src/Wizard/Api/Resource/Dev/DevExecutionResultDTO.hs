module Wizard.Api.Resource.Dev.DevExecutionResultDTO where

import GHC.Generics

data AdminExecutionResultDTO =
  AdminExecutionResultDTO
    { _devExecutionResultDTOOutput :: String
    }
  deriving (Show, Eq, Generic)
