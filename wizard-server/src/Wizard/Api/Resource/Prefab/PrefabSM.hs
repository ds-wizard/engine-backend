module Wizard.Api.Resource.Prefab.PrefabSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Common.AesonSM ()
import Wizard.Api.Resource.Prefab.PrefabJM ()
import Wizard.Database.Migration.Development.Prefab.Data.Prefabs
import Wizard.Model.Prefab.Prefab

instance ToSchema Prefab where
  declareNamedSchema = toSwagger kmIntegrationBioPortalPrefab
