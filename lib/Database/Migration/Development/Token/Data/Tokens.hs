module Database.Migration.Development.Token.Data.Tokens where

import Api.Resource.Token.TokenCreateDTO

albertCreateToken =
  TokenCreateDTO {_tokenCreateDTOEmail = "albert.einstein@example.com", _tokenCreateDTOPassword = "password"}
