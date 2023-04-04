module Shared.Api.Resource.Component.ComponentSM where

import Data.Swagger

import Shared.Api.Resource.Component.ComponentJM ()
import Shared.Database.Migration.Development.Component.Data.Components
import Shared.Model.Component.Component
import Shared.Util.Swagger

instance ToSchema Component where
  declareNamedSchema = toSwagger mailComponent
