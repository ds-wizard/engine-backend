module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO where

import GHC.Generics

data DocumentTemplateDraftCreateDTO = DocumentTemplateDraftCreateDTO
  { name :: String
  , templateId :: String
  , version :: String
  , basedOn :: Maybe String
  }
  deriving (Show, Eq, Generic)
