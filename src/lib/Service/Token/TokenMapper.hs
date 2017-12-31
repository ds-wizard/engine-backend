module Service.Token.TokenMapper where

import Api.Resource.Token.TokenDTO
import Common.Types

toDTO :: Token -> TokenDTO
toDTO token = TokenDTO {_tdtoToken = token}
