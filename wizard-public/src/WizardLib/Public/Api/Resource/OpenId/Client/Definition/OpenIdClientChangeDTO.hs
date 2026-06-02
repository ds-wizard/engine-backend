module WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO where

import GHC.Generics

import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Model.OpenId.OpenIdClientStyle

data OpenIdClientChangeDTO = OpenIdClientChangeDTO
  { name :: String
  , url :: String
  , clientId :: String
  , clientSecret :: String
  , parameters :: [OpenIdClientParameter]
  , style :: OpenIdClientStyle
  , registrationEnabled :: Bool
  , scopeProfile :: Bool
  , scopeEmail :: Bool
  }
  deriving (Generic, Show)
