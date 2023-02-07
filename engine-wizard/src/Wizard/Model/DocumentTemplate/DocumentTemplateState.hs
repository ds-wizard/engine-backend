module Wizard.Model.DocumentTemplate.DocumentTemplateState where

import GHC.Generics

data DocumentTemplateState
  = UnknownDocumentTemplateState
  | OutdatedDocumentTemplateState
  | UpToDateDocumentTemplateState
  | UnpublishedDocumentTemplateState
  | UnsupportedMetamodelVersionDocumentTemplateState
  deriving (Show, Eq, Generic, Read)
