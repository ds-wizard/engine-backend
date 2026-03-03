module Shared.Common.Api.Resource.Version.VersionDTO where

import qualified Data.UUID as U
import GHC.Generics

data VersionDTO = VersionDTO
  { uuid :: U.UUID
  , version :: String
  }
  deriving (Show, Eq, Generic)
