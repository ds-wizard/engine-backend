module Wizard.Api.Resource.Typehint.TypehintDTO where

import GHC.Generics

data TypehintDTO = TypehintDTO
  { intId :: Maybe String
  , name :: String
  , url :: Maybe String
  }
  deriving (Show, Eq, Generic)
