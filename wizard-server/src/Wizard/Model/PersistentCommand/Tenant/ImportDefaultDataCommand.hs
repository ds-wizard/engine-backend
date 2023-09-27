module Wizard.Model.PersistentCommand.Tenant.ImportDefaultDataCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data ImportDefaultDataCommand = ImportDefaultDataCommand
  { tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON ImportDefaultDataCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ImportDefaultDataCommand where
  toJSON = genericToJSON jsonOptions
