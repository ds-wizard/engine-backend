module Shared.Common.Api.Resource.Common.EntityCreatedWithIdDTO where

import GHC.Generics

data EntityCreatedWithIdDTO = EntityCreatedWithIdDTO
  { aId :: String
  }
  deriving (Show, Eq, Generic)
