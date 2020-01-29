module Wizard.Api.Resource.Document.DocumentCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentCreateDTO =
  DocumentCreateDTO
    { _documentCreateDTOName :: String
    , _documentCreateDTOQuestionnaireUuid :: U.UUID
    , _documentCreateDTOTemplateUuid :: U.UUID
    , _documentCreateDTOFormat :: String
    }
  deriving (Show, Eq, Generic)
