module WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateAuthenticationConfigCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateAuthenticationConfigCommand = CreateAuthenticationConfigCommand
  { aId :: String
  , name :: String
  , url :: String
  , clientId :: U.UUID
  , clientSecret :: String
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateAuthenticationConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateAuthenticationConfigCommand where
  toJSON = genericToJSON jsonOptions
