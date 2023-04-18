module Wizard.Service.Typehint.TypehintMapper where

import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Integration.Resource.Typehint.TypehintIDTO

toDTO :: String -> TypehintIDTO -> TypehintDTO
toDTO url iDto = TypehintDTO {intId = iDto.intId, name = iDto.name, url = url}
