module Shared.Database.Migration.Development.Component.Data.Components where

import Shared.Model.Component.Component
import Shared.Util.Date

mailComponent :: Component
mailComponent =
  Component
    { name = "Mail Component"
    , version = "1.0.0"
    , builtAt = dt' 2018 1 21
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
