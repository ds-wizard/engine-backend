module WizardLib.Public.Model.PersistentCommand.Config.CreateAppConfigAuthenticationCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateAppConfigAuthenticationCommand = CreateAppConfigAuthenticationCommand
  { aId :: String
  , name :: String
  , url :: String
  , clientId :: U.UUID
  , clientSecret :: String
  , appUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateAppConfigAuthenticationCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateAppConfigAuthenticationCommand where
  toJSON = genericToJSON jsonOptions
