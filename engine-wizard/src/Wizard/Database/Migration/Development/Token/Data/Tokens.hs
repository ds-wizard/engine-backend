module Wizard.Database.Migration.Development.Token.Data.Tokens where

import Wizard.Api.Resource.Token.TokenCreateDTO

albertCreateToken :: TokenCreateDTO
albertCreateToken =
  TokenCreateDTO {_tokenCreateDTOEmail = "albert.einstein@example.com", _tokenCreateDTOPassword = "password"}
