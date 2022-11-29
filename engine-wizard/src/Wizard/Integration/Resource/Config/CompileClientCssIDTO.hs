module Wizard.Integration.Resource.Config.CompileClientCssIDTO where

import GHC.Generics

data CompileClientCssIDTO = CompileClientCssIDTO
  { clientUrl :: String
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , illustrationsColor :: Maybe String
  }
  deriving (Show, Eq, Generic)
