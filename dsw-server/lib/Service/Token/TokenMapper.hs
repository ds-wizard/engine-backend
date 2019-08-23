module Service.Token.TokenMapper where

import Api.Resource.Token.TokenDTO
import Model.Token.Token

toDTO :: Token -> TokenDTO
toDTO token = TokenDTO {_tokenDTOToken = token}
