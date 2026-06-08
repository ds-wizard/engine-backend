module WizardLib.Public.Model.PersistentCommand.OpenId.CreateOrUpdateOpenIdClientDefinitionCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdateOpenIdClientDefinitionCommand = CreateOrUpdateOpenIdClientDefinitionCommand
  { uuid :: U.UUID
  , name :: String
  , url :: String
  , clientId :: String
  , clientSecret :: String
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdateOpenIdClientDefinitionCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdateOpenIdClientDefinitionCommand where
  toJSON = genericToJSON jsonOptions
