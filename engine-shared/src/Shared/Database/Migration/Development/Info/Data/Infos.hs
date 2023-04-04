module Shared.Database.Migration.Development.Info.Data.Infos where

import Shared.Api.Resource.Info.InfoDTO
import Shared.Database.Migration.Development.Component.Data.Components
import Shared.Util.Date

appInfo :: InfoDTO
appInfo =
  InfoDTO
    { name = "Engine"
    , version = "1.0.0"
    , builtAt = dt' 2018 1 21
    , components = [mailComponent]
    }
