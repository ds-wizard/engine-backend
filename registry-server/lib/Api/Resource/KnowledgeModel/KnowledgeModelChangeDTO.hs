module Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventDTO

data KnowledgeModelChangeDTO = KnowledgeModelChangeDTO
  { _knowledgeModelChangeDTOPackageId :: Maybe String
  , _knowledgeModelChangeDTOEvents :: [EventDTO]
  , _knowledgeModelChangeDTOTagUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)
