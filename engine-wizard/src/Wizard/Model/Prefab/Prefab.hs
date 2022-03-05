module Wizard.Model.Prefab.Prefab where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Prefab =
  Prefab
    { _prefabUuid :: U.UUID
    , _prefabPType :: String
    , _prefabName :: String
    , _prefabContent :: Value
    , _prefabAppUuid :: U.UUID
    , _prefabCreatedAt :: UTCTime
    , _prefabUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
