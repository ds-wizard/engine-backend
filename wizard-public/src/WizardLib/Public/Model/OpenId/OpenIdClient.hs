module WizardLib.Public.Model.OpenId.OpenIdClient where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Model.OpenId.OpenIdClientStyle

data OpenIdClient = OpenIdClient
  { uuid :: U.UUID
  , name :: String
  , url :: String
  , clientId :: String
  , clientSecret :: String
  , parameters :: [OpenIdClientParameter]
  , style :: OpenIdClientStyle
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , registrationEnabled :: Bool
  , scopeProfile :: Bool
  , scopeEmail :: Bool
  }
  deriving (Generic, Eq, Show)
