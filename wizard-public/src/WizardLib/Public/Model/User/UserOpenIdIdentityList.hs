module WizardLib.Public.Model.User.UserOpenIdIdentityList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.OpenId.Model.OpenId.OpenIdClientStyle

data UserOpenIdIdentityList = UserOpenIdIdentityList
  { uuid :: U.UUID
  , externalId :: String
  , externalLabel :: Maybe String
  , providerUuid :: U.UUID
  , providerName :: String
  , providerStyle :: OpenIdClientStyle
  , createdAt :: UTCTime
  }
  deriving (Generic, Show)
