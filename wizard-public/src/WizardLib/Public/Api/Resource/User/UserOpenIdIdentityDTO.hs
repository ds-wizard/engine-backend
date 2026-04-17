module WizardLib.Public.Api.Resource.User.UserOpenIdIdentityDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.OpenId.Model.OpenId.OpenIdClientStyle

data UserOpenIdIdentityDTO = UserOpenIdIdentityDTO
  { uuid :: U.UUID
  , externalId :: String
  , externalLabel :: Maybe String
  , providerUuid :: U.UUID
  , providerName :: String
  , providerStyle :: OpenIdClientStyle
  , createdAt :: UTCTime
  }
  deriving (Generic, Show)
