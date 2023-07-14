module WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdateUserCommand = CreateOrUpdateUserCommand
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , sources :: [String]
  , uRole :: String
  , active :: Bool
  , imageUrl :: Maybe String
  , appUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdateUserCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdateUserCommand where
  toJSON = genericToJSON jsonOptions
