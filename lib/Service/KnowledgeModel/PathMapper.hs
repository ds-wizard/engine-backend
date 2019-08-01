module Service.KnowledgeModel.PathMapper where

import Control.Lens ((^.))

import Api.Resource.KnowledgeModel.PathDTO
import LensesConfig
import Model.KnowledgeModel.Path

toPathItemDTO :: PathItem -> PathItemDTO
toPathItemDTO pathItem = PathItemDTO {_pathItemDTOPType = pathItem ^. pType, _pathItemDTOUuid = pathItem ^. uuid}

toPathDTO :: Path -> PathDTO
toPathDTO pathItems = toPathItemDTO <$> pathItems

fromPathItemDTO :: PathItemDTO -> PathItem
fromPathItemDTO dto = PathItem {_pathItemPType = dto ^. pType, _pathItemUuid = dto ^. uuid}

fromPathDTO :: PathDTO -> Path
fromPathDTO dto = fromPathItemDTO <$> dto
