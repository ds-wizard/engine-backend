module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent

data KnowledgeModelChangeDTO = KnowledgeModelChangeDTO
  { knowledgeModelPackageUuid :: Maybe U.UUID
  , events :: [KnowledgeModelEvent]
  , tagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
