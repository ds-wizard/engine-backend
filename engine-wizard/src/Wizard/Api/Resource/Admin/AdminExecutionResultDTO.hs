module Wizard.Api.Resource.Admin.AdminExecutionResultDTO where

import GHC.Generics

data AdminExecutionResultDTO =
  AdminExecutionResultDTO
    { _adminExecutionResultDTOOutput :: String
    }
  deriving (Show, Eq, Generic)
