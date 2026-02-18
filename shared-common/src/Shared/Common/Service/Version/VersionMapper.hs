module Shared.Common.Service.Version.VersionMapper where

import qualified Data.UUID as U

import Shared.Common.Api.Resource.Version.VersionDTO

toVersionDTO :: (U.UUID, String) -> VersionDTO
toVersionDTO (uuid, version) =
  VersionDTO
    { uuid = uuid
    , version = version
    }
