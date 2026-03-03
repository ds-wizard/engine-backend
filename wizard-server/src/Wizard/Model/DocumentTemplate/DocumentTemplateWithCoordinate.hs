module Wizard.Model.DocumentTemplate.DocumentTemplateWithCoordinate where

import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateWithCoordinate = DocumentTemplateWithCoordinate
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  }
  deriving (Show, Eq, Generic)
