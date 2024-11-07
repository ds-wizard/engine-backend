module Wizard.Model.PersistentCommand.Migration.Metamodel.MigrateToLatestMetamodelVersionCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data MigrateToLatestMetamodelVersionCommand = MigrateToLatestMetamodelVersionCommand
  { tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON MigrateToLatestMetamodelVersionCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigrateToLatestMetamodelVersionCommand where
  toJSON = genericToJSON jsonOptions
