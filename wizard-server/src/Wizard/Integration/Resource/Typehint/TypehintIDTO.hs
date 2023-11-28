module Wizard.Integration.Resource.Typehint.TypehintIDTO where

data TypehintIDTO = TypehintIDTO
  { intId :: Maybe String
  , name :: String
  }
  deriving (Show, Eq)
