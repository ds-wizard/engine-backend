module Api.Resource.KnowledgeModel.PathDTO where

import qualified Data.UUID as U
import GHC.Generics

data PathItemDTO = PathItemDTO
  { _pathItemDTOPType :: String
  , _pathItemDTOUuid :: U.UUID
  } deriving (Show, Eq, Generic)

type PathDTO = [PathItemDTO]
