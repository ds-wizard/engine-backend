module Shared.Component.Api.Resource.Component.ComponentSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Component.Api.Resource.Component.ComponentJM ()
import Shared.Component.Database.Migration.Development.Component.Data.Components
import Shared.Component.Model.Component.Component

instance ToSchema Component where
  declareNamedSchema = toSwagger mailComponent
