module Wizard.Database.Migration.Development.User.Data.UserTokens where

import Shared.Constant.App
import Shared.Util.Date
import Shared.Util.Uuid
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User
import Wizard.Model.User.UserToken
import Wizard.Service.UserToken.UserTokenMapper

albertToken :: UserToken
albertToken =
  UserToken
    { uuid = u' "b33aeab0-3fc5-4c10-9422-1f5e7b1480cc"
    , userUuid = userAlbert.uuid
    , value =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6ImIzM2FlYWIwLTNmYzUtNGMxMC05NDIyLTFmNWU3YjE0ODBjYyIsInVzZXJVdWlkIjoiZWM2ZjhlOTAtMmE5MS00OWVjLWFhM2YtOWVhYjIyNjdmYzY2IiwiZXhwIjoyNTMwMjcyMTE2fQ.JWcI1aouNXXRdRcFmIvHzy0QXChWcgbD1_bfJSlAFgw"
    , sessionState = Nothing
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

albertTokenDto :: UserTokenDTO
albertTokenDto = toDTO albertToken

albertCreateToken :: UserTokenCreateDTO
albertCreateToken =
  UserTokenCreateDTO {email = userAlbert.email, password = "password"}

alternativeAlbertToken :: UserToken
alternativeAlbertToken =
  UserToken
    { uuid = u' "50bac4a3-b373-431e-8cdb-320a3603f23a"
    , userUuid = userAlbert.uuid
    , value = "someOtherToken"
    , sessionState = Nothing
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

nikolaToken :: UserToken
nikolaToken =
  UserToken
    { uuid = u' "71deaa7d-af0f-40fd-a188-a5a1e5680ef4"
    , userUuid = userNikola.uuid
    , value =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6IjcxZGVhYTdkLWFmMGYtNDBmZC1hMTg4LWE1YTFlNTY4MGVmNCIsInVzZXJVdWlkIjoiMzBkNDhjZjQtOGM4YS00OTZmLWJhZmUtNTg1YmQyMzhmNzk4IiwiZXhwIjoyNTMwMjcyNjg5fQ.CcdBwHA7uJZgi_SK3ymOF_MwdHHpiHLxCtT6HUQs7Ew"
    , sessionState = Nothing
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

nikolaCreateToken :: UserTokenCreateDTO
nikolaCreateToken =
  UserTokenCreateDTO {email = userNikola.email, password = "password"}

isaacToken :: UserToken
isaacToken =
  UserToken
    { uuid = u' "64e6666e-ce73-441a-87b2-17887c86ef74"
    , userUuid = userIsaac.uuid
    , value =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6IjY0ZTY2NjZlLWNlNzMtNDQxYS04N2IyLTE3ODg3Yzg2ZWY3NCIsInVzZXJVdWlkIjoiZTFjNThlNTItMDgyNC00NTI2LThlYmUtZWMzOGVlYzY3MDMwIiwiZXhwIjoyNTMwMjcyNzg3fQ.MgDHvhrpiBqNxSwWfbyybWIEf8n6TdTHmmQeqOyejkE"
    , sessionState = Nothing
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

isaacCreateToken :: UserTokenCreateDTO
isaacCreateToken =
  UserTokenCreateDTO {email = userIsaac.email, password = "password"}
