module Wizard.Api.Resource.Typehint.TypehintDTO where

import GHC.Generics

data TypehintDTO = TypehintDTO
  { intId :: String
  , name :: String
  , url :: String
  }
  deriving (Show, Eq, Generic)
