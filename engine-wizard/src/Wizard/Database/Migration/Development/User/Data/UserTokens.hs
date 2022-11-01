module Wizard.Database.Migration.Development.User.Data.UserTokens where

import Control.Lens ((^.))

import LensesConfig
import Shared.Constant.App
import Shared.Util.Date
import Shared.Util.Uuid
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.UserToken
import Wizard.Service.UserToken.UserTokenMapper

albertToken :: UserToken
albertToken =
  UserToken
    { _userTokenUuid = u' "b33aeab0-3fc5-4c10-9422-1f5e7b1480cc"
    , _userTokenUserUuid = userAlbert ^. uuid
    , _userTokenValue =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6ImIzM2FlYWIwLTNmYzUtNGMxMC05NDIyLTFmNWU3YjE0ODBjYyIsInVzZXJVdWlkIjoiZWM2ZjhlOTAtMmE5MS00OWVjLWFhM2YtOWVhYjIyNjdmYzY2IiwiZXhwIjoyNTMwMjcyMTE2fQ.JWcI1aouNXXRdRcFmIvHzy0QXChWcgbD1_bfJSlAFgw"
    , _userTokenSessionState = Nothing
    , _userTokenAppUuid = defaultAppUuid
    , _userTokenCreatedAt = dt' 2022 1 21
    }

albertTokenDto :: UserTokenDTO
albertTokenDto = toDTO albertToken

albertCreateToken :: UserTokenCreateDTO
albertCreateToken =
  UserTokenCreateDTO {_userTokenCreateDTOEmail = userAlbert ^. email, _userTokenCreateDTOPassword = "password"}

alternativeAlbertToken :: UserToken
alternativeAlbertToken =
  UserToken
    { _userTokenUuid = u' "50bac4a3-b373-431e-8cdb-320a3603f23a"
    , _userTokenUserUuid = userAlbert ^. uuid
    , _userTokenValue = "someOtherToken"
    , _userTokenSessionState = Nothing
    , _userTokenAppUuid = defaultAppUuid
    , _userTokenCreatedAt = dt' 2022 1 21
    }

nikolaToken :: UserToken
nikolaToken =
  UserToken
    { _userTokenUuid = u' "71deaa7d-af0f-40fd-a188-a5a1e5680ef4"
    , _userTokenUserUuid = userNikola ^. uuid
    , _userTokenValue =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6IjcxZGVhYTdkLWFmMGYtNDBmZC1hMTg4LWE1YTFlNTY4MGVmNCIsInVzZXJVdWlkIjoiMzBkNDhjZjQtOGM4YS00OTZmLWJhZmUtNTg1YmQyMzhmNzk4IiwiZXhwIjoyNTMwMjcyNjg5fQ.CcdBwHA7uJZgi_SK3ymOF_MwdHHpiHLxCtT6HUQs7Ew"
    , _userTokenSessionState = Nothing
    , _userTokenAppUuid = defaultAppUuid
    , _userTokenCreatedAt = dt' 2022 1 21
    }

nikolaCreateToken :: UserTokenCreateDTO
nikolaCreateToken =
  UserTokenCreateDTO {_userTokenCreateDTOEmail = userNikola ^. email, _userTokenCreateDTOPassword = "password"}

isaacToken :: UserToken
isaacToken =
  UserToken
    { _userTokenUuid = u' "64e6666e-ce73-441a-87b2-17887c86ef74"
    , _userTokenUserUuid = userIsaac ^. uuid
    , _userTokenValue =
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoiMyIsInRva2VuVXVpZCI6IjY0ZTY2NjZlLWNlNzMtNDQxYS04N2IyLTE3ODg3Yzg2ZWY3NCIsInVzZXJVdWlkIjoiZTFjNThlNTItMDgyNC00NTI2LThlYmUtZWMzOGVlYzY3MDMwIiwiZXhwIjoyNTMwMjcyNzg3fQ.MgDHvhrpiBqNxSwWfbyybWIEf8n6TdTHmmQeqOyejkE"
    , _userTokenSessionState = Nothing
    , _userTokenAppUuid = defaultAppUuid
    , _userTokenCreatedAt = dt' 2022 1 21
    }

isaacCreateToken :: UserTokenCreateDTO
isaacCreateToken =
  UserTokenCreateDTO {_userTokenCreateDTOEmail = userIsaac ^. email, _userTokenCreateDTOPassword = "password"}
