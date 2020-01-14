module Wizard.Model.Document.DocumentTemplateContext where

import Data.Default
import Data.Text
import GHC.Generics

data DocumentTemplateContext =
  DocumentTemplateContext
    { _documentTemplateContextBaseURL :: Text
    , _documentTemplateContextResourcePageURL :: Text
    }
  deriving (Show, Generic)

instance Default DocumentTemplateContext where
  def =
    DocumentTemplateContext
      { _documentTemplateContextBaseURL = "https://ds-wizard.org"
      , _documentTemplateContextResourcePageURL = "https://researchers.ds-wizard.org/book-references/:shortuid"
      }
