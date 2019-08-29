module Model.Document.DocumentTemplateContext where

import Data.Default
import Data.Text
import GHC.Generics

data DocumentTemplateContext = DocumentTemplateContext
  { _documentTemplateContextBaseURL :: Text
  , _documentTemplateContextResourcePageURL :: Text
  } deriving (Show, Generic)

instance Default DocumentTemplateContext where
  def =
    DocumentTemplateContext
    { _documentTemplateContextBaseURL = "https://dsw.fairdata.solutions"
    , _documentTemplateContextResourcePageURL = "https://app.dsw.fairdata.solutions/book-references/:shortuid"
    }
