module Wizard.Api.Resource.ActionKey.ActionKeyDTO where

import GHC.Generics

import Wizard.Model.ActionKey.ActionKey

data ActionKeyDTO = ActionKeyDTO
  { aType :: ActionKeyType
  , email :: String
  }
  deriving (Show, Eq, Generic)
