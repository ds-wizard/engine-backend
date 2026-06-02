module WizardLib.Public.Model.OpenId.OpenIdClientSimple where

import qualified Data.UUID as U
import GHC.Generics

import Shared.OpenId.Model.OpenId.OpenIdClientStyle

data OpenIdClientSimple = OpenIdClientSimple
  { uuid :: U.UUID
  , name :: String
  , url :: String
  , style :: OpenIdClientStyle
  }
  deriving (Generic, Eq, Show)
