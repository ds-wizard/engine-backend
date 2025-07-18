module Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeDTO where

import GHC.Generics

data KnowledgeModelSecretChangeDTO = KnowledgeModelSecretChangeDTO
  { name :: String
  , value :: String
  }
  deriving (Show, Eq, Generic)
