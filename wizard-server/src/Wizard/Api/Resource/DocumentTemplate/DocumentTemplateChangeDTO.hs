module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO where

import GHC.Generics

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

data DocumentTemplateChangeDTO = DocumentTemplateChangeDTO
  { phase :: DocumentTemplatePhase
  }
  deriving (Show, Eq, Generic)
