module Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

data TypeHintTestRequestDTO = TypeHintTestRequestDTO
  { knowledgeModelEditorUuid :: U.UUID
  , integrationUuid :: U.UUID
  , q :: String
  , variables :: M.Map String String
  }
  deriving (Show, Eq, Generic)
