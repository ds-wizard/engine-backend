module Wizard.Database.Migration.Development.Prefab.Data.Prefabs where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.Prefab.Prefab

kmIntegrationBioPortalPrefab :: Prefab
kmIntegrationBioPortalPrefab =
  Prefab
    { _prefabUuid = u' "50a9af2b-d318-4ec1-ae4c-7fa9bbd4bace"
    , _prefabPType = "knowledge-model-integration"
    , _prefabName = "Bio Portal"
    , _prefabContent = toJSON $ HM.fromList [("key1", "value1"), ("key2", "value2")]
    , _prefabAppUuid = defaultApp ^. uuid
    , _prefabCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _prefabUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

authServicePrefab :: Prefab
authServicePrefab =
  Prefab
    { _prefabUuid = u' "7eb6ef27-d907-428c-bbc1-9b884a11d393"
    , _prefabPType = "authentication-service"
    , _prefabName = "Authentication Service"
    , _prefabContent = toJSON $ HM.fromList [("key1", 2), ("key2", 3)]
    , _prefabAppUuid = defaultApp ^. uuid
    , _prefabCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _prefabUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentPrefab :: Prefab
differentPrefab =
  Prefab
    { _prefabUuid = u' "50a9af2b-d318-4ec1-ae4c-7fa9bbd4bace"
    , _prefabPType = "knowledge-model-integration"
    , _prefabName = "Different Bio Portal"
    , _prefabContent = toJSON $ HM.fromList [("differentKey1", True), ("differentKey2", False)]
    , _prefabAppUuid = differentApp ^. uuid
    , _prefabCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _prefabUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }
