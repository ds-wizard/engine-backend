module WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateOrUpdateAuthenticationConfigCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdateAuthenticationConfigCommand = CreateOrUpdateAuthenticationConfigCommand
  { aId :: String
  , name :: String
  , url :: String
  , clientId :: U.UUID
  , clientSecret :: String
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdateAuthenticationConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdateAuthenticationConfigCommand where
  toJSON = genericToJSON jsonOptions
