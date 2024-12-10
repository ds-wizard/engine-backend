module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateDraftDataChangeDTO = DocumentTemplateDraftDataChangeDTO
  { questionnaireUuid :: Maybe U.UUID
  , branchUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
