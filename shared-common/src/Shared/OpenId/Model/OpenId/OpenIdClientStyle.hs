module Shared.OpenId.Model.OpenId.OpenIdClientStyle where

import GHC.Generics

data OpenIdClientStyle = OpenIdClientStyle
  { icon :: Maybe String
  , background :: Maybe String
  , color :: Maybe String
  }
  deriving (Generic, Eq, Show)