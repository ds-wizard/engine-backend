module Service.Level.LevelMapper where

import Control.Lens ((^.))

import Api.Resource.Level.LevelDTO
import LensesConfig
import Model.Level.Level

toLevelDTO :: Level -> LevelDTO
toLevelDTO l =
  LevelDTO
  { _levelDTOLevel = l ^. level
  , _levelDTOTitle = l ^. title
  , _levelDTODescription = l ^. description
  , _levelDTOCreatedAt = l ^. createdAt
  , _levelDTOUpdatedAt = l ^. updatedAt
  }
