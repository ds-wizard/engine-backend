module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO where

import GHC.Generics

data KnowledgeModelEditorChangeDTO = KnowledgeModelEditorChangeDTO
  { name :: String
  , kmId :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  }
  deriving (Generic)
