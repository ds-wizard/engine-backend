module Shared.Api.Resource.Info.InfoSM where

import Data.Swagger

import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Info.InfoJM ()
import Shared.Database.Migration.Development.Info.Data.Infos
import Shared.Util.Swagger

instance ToSchema InfoDTO where
  declareNamedSchema = simpleToSchema appInfo
