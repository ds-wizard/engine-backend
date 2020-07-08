module Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data KnowledgeModelChangeDTO =
  KnowledgeModelChangeDTO
    { _knowledgeModelChangeDTOPackageId :: Maybe String
    , _knowledgeModelChangeDTOEvents :: [Event]
    , _knowledgeModelChangeDTOTagUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)
