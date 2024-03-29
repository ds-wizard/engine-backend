module WizardLib.Public.Model.PersistentCommand.Tenant.Plan.DeletePlanCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data DeletePlanCommand = DeletePlanCommand
  { uuid :: U.UUID
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeletePlanCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeletePlanCommand where
  toJSON = genericToJSON jsonOptions
