module WizardLib.Public.Model.PersistentCommand.Plan.CreateOrUpdatePlanCommand where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdatePlanCommand = CreateOrUpdatePlanCommand
  { uuid :: U.UUID
  , name :: String
  , users :: Maybe Int
  , since :: Maybe UTCTime
  , until :: Maybe UTCTime
  , test :: Bool
  , appUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdatePlanCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdatePlanCommand where
  toJSON = genericToJSON jsonOptions
