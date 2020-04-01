module Wizard.Database.Migration.Development.Token.Data.Tokens where

import Wizard.Api.Resource.Token.TokenCreateDTO
import Wizard.Api.Resource.Token.TokenDTO

albertCreateToken :: TokenCreateDTO
albertCreateToken =
  TokenCreateDTO {_tokenCreateDTOEmail = "albert.einstein@example.com", _tokenCreateDTOPassword = "password"}

albertToken :: TokenDTO
albertToken = TokenDTO {_tokenDTOToken = "abc"}
