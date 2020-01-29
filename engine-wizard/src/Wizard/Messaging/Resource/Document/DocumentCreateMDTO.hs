module Wizard.Messaging.Resource.Document.DocumentCreateMDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Document.DocumentContextDTO

data DocumentCreateMDTO =
  DocumentCreateMDTO
    { _documentCreateMDTODocumentUuid :: U.UUID
    , _documentCreateMDTODocumentContext :: DocumentContextDTO
    }
  deriving (Show, Eq, Generic)
