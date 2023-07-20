module Shared.Common.Database.Migration.Development.Info.Data.Infos where

import Shared.Common.Api.Resource.Info.InfoDTO
import Shared.Common.Util.Date
import Shared.Component.Database.Migration.Development.Component.Data.Components

appInfo :: InfoDTO
appInfo =
  InfoDTO
    { name = "Engine"
    , version = "1.0.0"
    , builtAt = dt' 2018 1 21
    , components = [mailComponent]
    }
