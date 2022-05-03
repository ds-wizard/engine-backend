module Wizard.Api.Resource.Dev.DevExecutionDTO where

import GHC.Generics

data DevExecutionDTO =
  DevExecutionDTO
    { _devExecutionDTOSectionName :: String
    , _devExecutionDTOOperationName :: String
    , _devExecutionDTOParameters :: [String]
    }
  deriving (Show, Eq, Generic)
