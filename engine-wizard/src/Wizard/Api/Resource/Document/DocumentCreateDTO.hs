module Wizard.Api.Resource.Document.DocumentCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentCreateDTO =
  DocumentCreateDTO
    { _documentCreateDTOName :: String
    , _documentCreateDTOQuestionnaireUuid :: U.UUID
    , _documentCreateDTOTemplateId :: String
    , _documentCreateDTOFormatUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
