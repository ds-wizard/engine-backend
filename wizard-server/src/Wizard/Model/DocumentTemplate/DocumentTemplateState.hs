module Wizard.Model.DocumentTemplate.DocumentTemplateState where

import GHC.Generics

data DocumentTemplateState
  = DefaultDocumentTemplateState
  | UnsupportedMetamodelVersionDocumentTemplateState
  deriving (Show, Eq, Generic, Read)
