module Wizard.Service.Typehint.TypehintMapper where

import Control.Lens ((^.))

import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.LensesConfig

toDTO :: String -> TypehintIDTO -> TypehintDTO
toDTO url iDto = TypehintDTO {_typehintDTOIntId = iDto ^. intId, _typehintDTOName = iDto ^. name, _typehintDTOUrl = url}
