module WizardLib.Public.Model.PersistentCommand.Trigger.TriggerEntityUuidCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data TriggerEntityUuidCommand = TriggerEntityUuidCommand
  { uuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON TriggerEntityUuidCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TriggerEntityUuidCommand where
  toJSON = genericToJSON jsonOptions
