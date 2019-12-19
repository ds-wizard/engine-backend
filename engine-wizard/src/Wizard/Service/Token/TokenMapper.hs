module Wizard.Service.Token.TokenMapper where

import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Model.Token.Token

toDTO :: Token -> TokenDTO
toDTO token = TokenDTO {_tokenDTOToken = token}
