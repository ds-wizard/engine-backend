module Service.Typehint.TypehintMapper where

import Control.Lens ((^.))

import Api.Resource.Typehint.TypehintDTO
import Integration.Resource.Typehint.TypehintIDTO
import LensesConfig

toDTO :: String -> TypehintIDTO -> TypehintDTO
toDTO url iDto = TypehintDTO {_typehintDTOIntId = iDto ^. intId, _typehintDTOName = iDto ^. name, _typehintDTOUrl = url}
