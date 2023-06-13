module Wizard.Api.Resource.Auth.AuthConsentDTO where

import GHC.Generics

data AuthConsentDTO = AuthConsentDTO
  { hash :: String
  , sessionState :: Maybe String
  }
  deriving (Generic)
