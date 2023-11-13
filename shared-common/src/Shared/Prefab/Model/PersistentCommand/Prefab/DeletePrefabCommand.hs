module Shared.Prefab.Model.PersistentCommand.Prefab.DeletePrefabCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data DeletePrefabCommand = DeletePrefabCommand
  { uuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeletePrefabCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeletePrefabCommand where
  toJSON = genericToJSON jsonOptions
