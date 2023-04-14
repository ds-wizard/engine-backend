module Wizard.Database.Migration.Development.User.Data.UserTokens where

import Shared.Constant.App
import Shared.Util.Date
import Shared.Util.Uuid
import Wizard.Api.Resource.UserToken.ApiKeyCreateDTO
import Wizard.Api.Resource.UserToken.LoginDTO
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User
import Wizard.Model.User.UserToken
import Wizard.Service.UserToken.UserTokenMapper

albertToken :: UserToken
albertToken =
  UserToken
    { uuid = u' "b33aeab0-3fc5-4c10-9422-1f5e7b1480cc"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userAlbert.uuid
    , value =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6ImIzM2FlYWIwLTNmYzUtNGMxMC05NDIyLTFmNWU3YjE0ODBjYyIsInVzZXJVdWlkIjoiZWM2ZjhlOTAtMmE5MS00OWVjLWFhM2YtOWVhYjIyNjdmYzY2IiwiZXhwIjoyNTMwMjcyMTE2fQ.JWcI1aouNXXRdRcFmIvHzy0QXChWcgbD1_bfJSlAFgw"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

albertTokenDto :: UserTokenDTO
albertTokenDto = toDTO albertToken

albertCreateToken :: LoginDTO
albertCreateToken =
  LoginDTO {email = userAlbert.email, password = "password", code = Nothing}

albertApiKey :: UserToken
albertApiKey =
  UserToken
    { uuid = u' "8591dd6f-c114-457a-9781-7411fc71e468"
    , name = "My API Key"
    , tType = ApiKeyUserTokenType
    , userUuid = userAlbert.uuid
    , value =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6ImIzM2FlYWIwLTNmYzUtNGMxMC05NDIyLTFmNWU3YjE0ODBjYyIsInVzZXJVdWlkIjoiZWM2ZjhlOTAtMmE5MS00OWVjLWFhM2YtOWVhYjIyNjdmYzY2IiwiZXhwIjoyNTMwMjcyMTE2fQ.JWcI1aouNXXRdRcFmIvHzy0QXChWcgbD1_bfJSlAFgw"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

albertCreateApiKey :: ApiKeyCreateDTO
albertCreateApiKey =
  ApiKeyCreateDTO {name = albertApiKey.name, expiresAt = dt' 2052 1 21}

alternativeAlbertToken :: UserToken
alternativeAlbertToken =
  UserToken
    { uuid = u' "50bac4a3-b373-431e-8cdb-320a3603f23a"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userAlbert.uuid
    , value = "someOtherToken"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

nikolaToken :: UserToken
nikolaToken =
  UserToken
    { uuid = u' "71deaa7d-af0f-40fd-a188-a5a1e5680ef4"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userNikola.uuid
    , value =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6IjcxZGVhYTdkLWFmMGYtNDBmZC1hMTg4LWE1YTFlNTY4MGVmNCIsInVzZXJVdWlkIjoiMzBkNDhjZjQtOGM4YS00OTZmLWJhZmUtNTg1YmQyMzhmNzk4IiwiZXhwIjoyNTMwMjcyNjg5fQ.CcdBwHA7uJZgi_SK3ymOF_MwdHHpiHLxCtT6HUQs7Ew"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

nikolaCreateToken :: LoginDTO
nikolaCreateToken =
  LoginDTO {email = userNikola.email, password = "password", code = Nothing}

isaacToken :: UserToken
isaacToken =
  UserToken
    { uuid = u' "64e6666e-ce73-441a-87b2-17887c86ef74"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userIsaac.uuid
    , value =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6IjY0ZTY2NjZlLWNlNzMtNDQxYS04N2IyLTE3ODg3Yzg2ZWY3NCIsInVzZXJVdWlkIjoiZTFjNThlNTItMDgyNC00NTI2LThlYmUtZWMzOGVlYzY3MDMwIiwiZXhwIjoyNTMwMjcyNzg3fQ.MgDHvhrpiBqNxSwWfbyybWIEf8n6TdTHmmQeqOyejkE"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

isaacCreateToken :: LoginDTO
isaacCreateToken =
  LoginDTO {email = userIsaac.email, password = "password", code = Nothing}
