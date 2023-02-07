module Wizard.Api.Resource.Document.DocumentCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentCreateDTO = DocumentCreateDTO
  { name :: String
  , questionnaireUuid :: U.UUID
  , questionnaireEventUuid :: Maybe U.UUID
  , documentTemplateId :: String
  , formatUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
