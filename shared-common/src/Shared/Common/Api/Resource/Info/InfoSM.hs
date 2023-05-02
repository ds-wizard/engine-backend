module Shared.Common.Api.Resource.Info.InfoSM where

import Data.Swagger

import Shared.Common.Api.Resource.Component.ComponentSM ()
import Shared.Common.Api.Resource.Info.InfoDTO
import Shared.Common.Api.Resource.Info.InfoJM ()
import Shared.Common.Database.Migration.Development.Info.Data.Infos
import Shared.Common.Util.Swagger

instance ToSchema InfoDTO where
  declareNamedSchema = toSwagger appInfo
