module Wizard.Model.PersistentCommand.App.ImportDefaultDataCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data ImportDefaultDataCommand = ImportDefaultDataCommand
  { appUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON ImportDefaultDataCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ImportDefaultDataCommand where
  toJSON = genericToJSON jsonOptions
