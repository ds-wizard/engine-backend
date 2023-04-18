module Shared.Common.Api.Resource.Component.ComponentSM where

import Data.Swagger

import Shared.Common.Api.Resource.Component.ComponentJM ()
import Shared.Common.Database.Migration.Development.Component.Data.Components
import Shared.Common.Model.Component.Component
import Shared.Common.Util.Swagger

instance ToSchema Component where
  declareNamedSchema = toSwagger mailComponent
