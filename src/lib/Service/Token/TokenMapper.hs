module Service.Token.TokenMapper where

import Api.Resources.Token.TokenDTO
import Common.Types

toDTO :: Token -> TokenDTO
toDTO token = TokenDTO {_tdtoToken = token}
