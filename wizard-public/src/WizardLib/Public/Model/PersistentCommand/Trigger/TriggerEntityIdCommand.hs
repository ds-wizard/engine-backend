module WizardLib.Public.Model.PersistentCommand.Trigger.TriggerEntityIdCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data TriggerEntityIdCommand = TriggerEntityIdCommand
  { aId :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON TriggerEntityIdCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TriggerEntityIdCommand where
  toJSON = genericToJSON jsonOptions
