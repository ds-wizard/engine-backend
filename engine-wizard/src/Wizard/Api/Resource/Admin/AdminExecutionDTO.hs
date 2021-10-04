module Wizard.Api.Resource.Admin.AdminExecutionDTO where

import GHC.Generics

data AdminExecutionDTO =
  AdminExecutionDTO
    { _adminExecutionDTOSectionName :: String
    , _adminExecutionDTOOperationName :: String
    , _adminExecutionDTOParameters :: [String]
    }
  deriving (Show, Eq, Generic)
