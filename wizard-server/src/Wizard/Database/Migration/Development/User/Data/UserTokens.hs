module Wizard.Database.Migration.Development.User.Data.UserTokens where

import Shared.Common.Constant.App
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.UserToken.ApiKeyCreateDTO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User
import Wizard.Service.UserToken.UserTokenMapper
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Model.User.UserToken

albertToken :: UserToken
albertToken =
  UserToken
    { uuid = u' "b33aeab0-3fc5-4c10-9422-1f5e7b1480cc"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userAlbert.uuid
    , value =
        "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MTg2ODU5MzAsInRva2VuVXVpZCI6ImIzM2FlYWIwLTNmYzUtNGMxMC05NDIyLTFmNWU3YjE0ODBjYyIsInVzZXJVdWlkIjoiZWM2ZjhlOTAtMmE5MS00OWVjLWFhM2YtOWVhYjIyNjdmYzY2IiwidmVyc2lvbiI6M30.MOwpUkwCRphqhtq7yycoK7ui3BhMxbpCF09Kqb4ihaT_9wjgWuzBCNgxq-M0TFUdyW5Ea8ozIPFv8cpy4lLBrvr-bb96U37oVQrNn8wrnuceD1KZJA616RlVXL9bKNGwZQF8vsyNbVD7Dosf97kKQh_9jA9iVS5LCqkyB0ethNSVqE-Az7V9ggkVHkezf55G7uvga8PmruwfDqt1lyxiWWREoYU6ox6yjhR9wxM6BBD9mkEiU5YBPsL5iNuqhrWNRx8Sl2u3JrwZp5_lRHNXHsErGJ7oH0gJHhb5O2Nt7wm78dNzyoWQ8FHkRdFvQU6lU-uuuFq8lIkz7aOiEpRblYo-5YpV3zPsO_WBTcG7jT-UDh2KbeNDfjcETnSlvC5sr-GT2JwyEPRod-VQNzqc4ZGkJGupmlCM3us2fNmY5HzXdjrTVK3c-u2ZUEg9IuvGz3670irA4mSBMjnCVeXoDholoixskriB9PkbRytkISxK53Ivuz7SUMjwymwzM3nzUSRED4LYC7hGHdfTFp3zV2UrAiYvCzIMWtY6MV6cAdsrO5JuH_ZlFxifqLT0yf0XP83CUkVsNkwgKxH0jgIKGahx7szGUgSrpJnFLv0ZTZU1Amp6qaxLdd0MxMWdFXrJ9-yjGs_teknZNmQZp8ueRtjbpy4Kq40RG5AoWB9IIas"
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
        "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MTg2ODU5MzAsInRva2VuVXVpZCI6ImIzM2FlYWIwLTNmYzUtNGMxMC05NDIyLTFmNWU3YjE0ODBjYyIsInVzZXJVdWlkIjoiZWM2ZjhlOTAtMmE5MS00OWVjLWFhM2YtOWVhYjIyNjdmYzY2IiwidmVyc2lvbiI6M30.MOwpUkwCRphqhtq7yycoK7ui3BhMxbpCF09Kqb4ihaT_9wjgWuzBCNgxq-M0TFUdyW5Ea8ozIPFv8cpy4lLBrvr-bb96U37oVQrNn8wrnuceD1KZJA616RlVXL9bKNGwZQF8vsyNbVD7Dosf97kKQh_9jA9iVS5LCqkyB0ethNSVqE-Az7V9ggkVHkezf55G7uvga8PmruwfDqt1lyxiWWREoYU6ox6yjhR9wxM6BBD9mkEiU5YBPsL5iNuqhrWNRx8Sl2u3JrwZp5_lRHNXHsErGJ7oH0gJHhb5O2Nt7wm78dNzyoWQ8FHkRdFvQU6lU-uuuFq8lIkz7aOiEpRblYo-5YpV3zPsO_WBTcG7jT-UDh2KbeNDfjcETnSlvC5sr-GT2JwyEPRod-VQNzqc4ZGkJGupmlCM3us2fNmY5HzXdjrTVK3c-u2ZUEg9IuvGz3670irA4mSBMjnCVeXoDholoixskriB9PkbRytkISxK53Ivuz7SUMjwymwzM3nzUSRED4LYC7hGHdfTFp3zV2UrAiYvCzIMWtY6MV6cAdsrO5JuH_ZlFxifqLT0yf0XP83CUkVsNkwgKxH0jgIKGahx7szGUgSrpJnFLv0ZTZU1Amp6qaxLdd0MxMWdFXrJ9-yjGs_teknZNmQZp8ueRtjbpy4Kq40RG5AoWB9IIas"
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
        "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MTg2ODY1MTMsInRva2VuVXVpZCI6IjcxZGVhYTdkLWFmMGYtNDBmZC1hMTg4LWE1YTFlNTY4MGVmNCIsInVzZXJVdWlkIjoiMzBkNDhjZjQtOGM4YS00OTZmLWJhZmUtNTg1YmQyMzhmNzk4IiwidmVyc2lvbiI6M30.rGTekQWgbz8VS1Ex1KBh4CQDtgxsX4n4bemRiUXBZfpSY47LicOubG-wnb-LWhzpHu7qEkd7DLJ5UFUStKofRoj-Aq3TWHVnQVOLydWhqC-k3ki2f7a3xU2NzpxhGXH-OhKCz1DN21oczUzAxuTBDHJ4DjWgTq9p1r38WxMNCOH5QHApAG0h48fVXBrlo-g82i8TOHdiFnR05phRJnlmXkSp2x5nAbp4nkIPALoAw7Jy6T05NpRKG5m7UpCFPAAWx_WhQ0xt5pT_Yv5qOjIRLbgEevX5oqPIXkPXffAmJGuqEek9RLOyiXKHMoQHuc2ozpXUKisKq3vEHoui8Y0ypAJfvq5P92bJtYxKcucsrNH8800R7MwD84jAVa9hK3GKvogPR6sZCVqCK8eLg95zUabqJroPCc42ON_iIdIsfIFAg9iB3T0t20qo7jsJFf5L0w8F376zPWUCDN3iJWQ1qLqjy16GyB8ypIg3rNauCd6FNwVAEL7BjSmIw2WqdU6FJPvuRsHL7kUjhS89v_R_ORMdxBNe66MnvXL83tXlApxTnlSZK3Jac8yePOOPruLxFGYe32xShtNCe-2HEHDodbniavGipTpbuLV2ZxR41WWPKPu9-N-gGINVoOkmhcizc1faYgbc9JnTmOHsK7KdEGiFubCjq37426IiLMAMJG8"
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
        "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MTg2ODY2MDYsInRva2VuVXVpZCI6IjY0ZTY2NjZlLWNlNzMtNDQxYS04N2IyLTE3ODg3Yzg2ZWY3NCIsInVzZXJVdWlkIjoiZTFjNThlNTItMDgyNC00NTI2LThlYmUtZWMzOGVlYzY3MDMwIiwidmVyc2lvbiI6M30.Sbis0EkW9dWtqiIJVeK1xjkP-Zptth1TRxG8vkLUtSXMJANY1Xiy6g_cw9K1d1jTSAYvkSlXOv6vNveQGnXYUF44G21bFIr8V3kjieKXfL0WeQ0E9T-XfIG1hULkLF23FEgzJ7lPSDFXlcHzTxnDjSVvrmnPoomhuC9gcNI7K5sYjhey1EO2lV9_Boe_I-rurvNyHeIvK4L89LkdcZlV087AIKrZaUNzCAteBZv931u5OxTcbbpPBZ6-cAmNtcy4j6Oe_EopTfPJ_rE5ZxzzGOs81cJz6MBk1Kw9Ya51AwTb4oYBTWgFVDgnb774H9aSMZW7dv9k-iDK2r-S-5Um8d_J0jCC5IaK4xWC8gjekM71cVPdvjIclcVs9SssZshRUngNc1hq-RsB5hluRJRms3dCgOyi-TExX_XWUsxkuH3IHAytn6n2GXC29okHXDcbTL5u4hBXC7bK0ErGg4CgcTYhNlp1FZNDH30wFzwoLyy81fqfkXgSMS0Ky5S5_k09umhljMHKCmN3QtADQFgFRPZC6QWPaiO1FgjOI8WK2PuMS-YxCatJa6z-lMGIffrTWgoh9d-lX_gFVQRbciUT787n9K-xxCtsgadAAjKbolj1PE3a8thnd3_0J5bjFFzDOB_DoUj3gWPCvFNrFBbKBzDsSdjFrAnQp9ufZFvhJIM"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , appUuid = defaultAppUuid
    , createdAt = dt' 2022 1 21
    }

isaacCreateToken :: LoginDTO
isaacCreateToken =
  LoginDTO {email = userIsaac.email, password = "password", code = Nothing}
