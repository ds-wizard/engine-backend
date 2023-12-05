module Wizard.Integration.Resource.Typehint.TypehintIDTO where

import GHC.Generics

data TypehintIDTO = TypehintIDTO
  { intId :: Maybe String
  , name :: String
  }
  deriving (Show, Eq, Generic)
