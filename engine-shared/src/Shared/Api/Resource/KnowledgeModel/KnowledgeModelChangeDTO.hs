module Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data KnowledgeModelChangeDTO = KnowledgeModelChangeDTO
  { packageId :: Maybe String
  , events :: [Event]
  , tagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
