module Shared.Prefab.Model.Prefab.Prefab where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Prefab = Prefab
  { uuid :: U.UUID
  , pType :: String
  , name :: String
  , content :: Value
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
