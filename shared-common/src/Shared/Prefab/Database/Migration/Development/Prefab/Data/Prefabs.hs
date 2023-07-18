module Shared.Prefab.Database.Migration.Development.Prefab.Data.Prefabs where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Constant.App
import Shared.Common.Util.Uuid
import Shared.Prefab.Model.Prefab.Prefab

kmIntegrationBioPortalPrefab :: Prefab
kmIntegrationBioPortalPrefab =
  Prefab
    { uuid = u' "50a9af2b-d318-4ec1-ae4c-7fa9bbd4bace"
    , pType = "knowledge-model-integration"
    , name = "Bio Portal"
    , content = toJSON $ HM.fromList [("key1", "value1"), ("key2", "value2")]
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

authServicePrefab :: Prefab
authServicePrefab =
  Prefab
    { uuid = u' "7eb6ef27-d907-428c-bbc1-9b884a11d393"
    , pType = "authentication-service"
    , name = "Authentication Service"
    , content = toJSON $ HM.fromList [("key1", 2), ("key2", 3)]
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentPrefab :: Prefab
differentPrefab =
  Prefab
    { uuid = u' "50a9af2b-d318-4ec1-ae4c-7fa9bbd4bace"
    , pType = "knowledge-model-integration"
    , name = "Different Bio Portal"
    , content = toJSON $ HM.fromList [("differentKey1", True), ("differentKey2", False)]
    , appUuid = differentAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }
