module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO where

import GHC.Generics

data KnowledgeModelEditorCreateDTO = KnowledgeModelEditorCreateDTO
  { name :: String
  , kmId :: String
  , version :: String
  , previousPackageId :: Maybe String
  }
  deriving (Generic)
