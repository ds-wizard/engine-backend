module Wizard.Integration.Resource.Config.CompileClientCssIDTO where

import GHC.Generics

data CompileClientCssIDTO =
  CompileClientCssIDTO
    { _compileClientCssIDTOClientUrl :: String
    , _compileClientCssIDTOLogoUrl :: Maybe String
    , _compileClientCssIDTOPrimaryColor :: Maybe String
    , _compileClientCssIDTOIllustrationsColor :: Maybe String
    }
  deriving (Show, Eq, Generic)
