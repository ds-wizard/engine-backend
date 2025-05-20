module WizardLib.Public.Model.PersistentCommand.User.Tour.DeleteToursCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data DeleteToursCommand = DeleteToursCommand
  { userUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeleteToursCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteToursCommand where
  toJSON = genericToJSON jsonOptions
