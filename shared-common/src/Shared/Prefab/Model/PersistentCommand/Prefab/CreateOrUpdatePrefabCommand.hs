module Shared.Prefab.Model.PersistentCommand.Prefab.CreateOrUpdatePrefabCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdatePrefabCommand = CreateOrUpdatePrefabCommand
  { uuid :: U.UUID
  , pType :: String
  , name :: String
  , content :: Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdatePrefabCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdatePrefabCommand where
  toJSON = genericToJSON jsonOptions
