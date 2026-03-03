module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelEditorCreateDTO = KnowledgeModelEditorCreateDTO
  { name :: String
  , kmId :: String
  , version :: String
  , previousPackageUuid :: Maybe U.UUID
  }
  deriving (Generic)
