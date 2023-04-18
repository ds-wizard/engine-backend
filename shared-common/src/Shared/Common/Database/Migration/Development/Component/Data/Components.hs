module Shared.Common.Database.Migration.Development.Component.Data.Components where

import Shared.Common.Model.Component.Component
import Shared.Common.Util.Date

mailComponent :: Component
mailComponent =
  Component
    { name = "Mail Component"
    , version = "1.0.0"
    , builtAt = dt' 2018 1 21
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
