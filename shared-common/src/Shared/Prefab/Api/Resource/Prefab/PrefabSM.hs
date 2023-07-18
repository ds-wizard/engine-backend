module Shared.Prefab.Api.Resource.Prefab.PrefabSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Util.Swagger
import Shared.Prefab.Api.Resource.Prefab.PrefabJM ()
import Shared.Prefab.Database.Migration.Development.Prefab.Data.Prefabs
import Shared.Prefab.Model.Prefab.Prefab

instance ToSchema Prefab where
  declareNamedSchema = toSwagger kmIntegrationBioPortalPrefab
