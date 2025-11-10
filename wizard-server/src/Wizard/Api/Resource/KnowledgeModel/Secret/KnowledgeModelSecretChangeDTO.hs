module Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO where

import GHC.Generics

data KnowledgeModelSecretChangeDTO = KnowledgeModelSecretChangeDTO
  { name :: String
  , value :: String
  }
  deriving (Show, Eq, Generic)
