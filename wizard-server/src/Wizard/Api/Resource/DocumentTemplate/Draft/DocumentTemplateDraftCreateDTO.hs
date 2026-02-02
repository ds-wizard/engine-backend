module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateDraftCreateDTO = DocumentTemplateDraftCreateDTO
  { name :: String
  , templateId :: String
  , version :: String
  , basedOn :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
