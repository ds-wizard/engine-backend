module Wizard.Model.App.ImportDefaultDataCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.JSON

data ImportDefaultDataCommand =
  ImportDefaultDataCommand
    { _importDefaultDataCommandAppUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

instance FromJSON ImportDefaultDataCommand where
  parseJSON = simpleParseJSON "_importDefaultDataCommand"

instance ToJSON ImportDefaultDataCommand where
  toJSON = simpleToJSON "_importDefaultDataCommand"
