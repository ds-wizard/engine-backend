module Wizard.Database.Migration.Development.User.Data.UserTokens where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User
import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.AppKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.UserToken.UserTokenMapper

albertToken :: UserToken
albertToken =
  UserToken
    { uuid = u' "54ea072d-b5f9-4251-b3a4-ae177360509c"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userAlbert.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE4MDgxODUyNjAsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI1NGVhMDcyZC1iNWY5LTQyNTEtYjNhNC1hZTE3NzM2MDUwOWMiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjR9.yFC4IAevnda7k8m03BR2o6y2i4TJBijnbKh4Hw0zVC5BfGy7NNgavCqgNaISKnQBITMP_nRZS0IW8tssp_fN3KxYXvOkhlxCBBwbrLtOj7Jfy4PUev3zkKvV33KJUi7_7rU-XLIZ0lIm3hL62ueVt5FvQC8_0Wk--Q_6GtwhZjm-U8cnNYl2NRuQrVyWl4OCQIfjks6wN7yjUQ4Ej3t80r4xr4Pbxk2qcDikkM_evp0U7dxWoI2QGGSFi7RFQ4Y6_WpfQO5_mqUQTRTZL0jnMJwXT_bxv4Fm6NZ6SUuliaFRxOE1Mbeffmy9qxhf0pbD3mr9OJ90pcG_sUEUshWTPDTzxlMAgLWwB4BMx1ip-x4IUCNW-_-VhdWXlLs4asH3QH5qAXGB_r6X4XYAm4IcApxACnCZXT6bk6DzOv8wqGmhKSTW0AcKPHyblib4ywuLfXvpcGy27ihigGR9HWAyn-MIF_QfTPkk_Mkk7Olc8e0Ayh8rFvZ0obni8qbuvOckaEkGm2rMK-owqeOHtDp2qC91tsoHi8cqKEMfY2p3z6m_P0sMOmc_L-9J7__TS54IRuU-ODhr9ZK4qUCoLJ_2zLyuHuoovbCTk3o9Y_-HcTez9x3ZinYD9xYx2jBWruf-4l57qbzb53EcHHKV7cCXyyAPKTgY6tJR6dEVBvRzgg0" -- cspell:disable-line
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
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
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzU4ODQyOTAsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI0M2Q0NGM2YS1kMjJlLTQ5NTQtYmVlNi0wZmVkYzhjOTkwZGYiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjR9.I39ofNurjfjDgeCgh3zFfrK6tC4KEQSOnxmvxY8lWFoO1szsKqKzWXQPfAnJzZF6tw6hS2Fj3p6xtvjHT-dzMcfP8DDzTO8bUua0l86u1jsVyNoqzM1V1SWcvfALah7_6WFcO9izXKvYV1VyL8ouTkaiTNxc365qeKFru6_wH5ts9vz3wcwvJaimeUEsacEacZXpmZGhs8zjVhzw9fluTyu_gOrx-58ci2JQJfQbP0JBfGhfhIxaVqBR25E-U8Zn1Tut-2mYHIcHx99iO6mbtllfOZHaUuwrrQYc6SLjY7ZNXFTFsjoagEVOdjeaU8MKLHvGum3cV7m7UVq8QMNi3KpRrLB7JRW8zG-GWZ1MRCRPrY9Otuevsb-jldQpJ6u-0fboYKHrJBIaQoCT0kEbtCS-hkgWeoCdezs-J94tJ7X5qNpjNrGl3aL6Z1xu01QH0DqpPKUatVPFleGcaaJmNWASHZWxN1_YZ6ZdlsYwFeiOm7MRGp8eY_LL0zDhJXEye0ridb0FG_PTbea_ztEbrcEulDffZWMhW4mJjkvDvrgwAvbkOMVfZJUwjbEsxp58KfRaJ6LTdIMSy69UNDl0k7HqF_v1Un84HchhZUmd_8nNV_EezMmV9an1wmzzjmQ0PhoZP0JQ05u96s7ciJo6mt0m8kz9YuYrHL0SN1HMA_E" -- cspell:disable-line
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

myAppKey :: UserToken
myAppKey =
  UserToken
    { uuid = u' "8591dd6f-c114-457a-9781-7411fc71e468"
    , name = "My App Key"
    , tType = AppKeyUserTokenType
    , userUuid = userAlbert.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzU4ODQyOTAsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI0M2Q0NGM2YS1kMjJlLTQ5NTQtYmVlNi0wZmVkYzhjOTkwZGYiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjR9.I39ofNurjfjDgeCgh3zFfrK6tC4KEQSOnxmvxY8lWFoO1szsKqKzWXQPfAnJzZF6tw6hS2Fj3p6xtvjHT-dzMcfP8DDzTO8bUua0l86u1jsVyNoqzM1V1SWcvfALah7_6WFcO9izXKvYV1VyL8ouTkaiTNxc365qeKFru6_wH5ts9vz3wcwvJaimeUEsacEacZXpmZGhs8zjVhzw9fluTyu_gOrx-58ci2JQJfQbP0JBfGhfhIxaVqBR25E-U8Zn1Tut-2mYHIcHx99iO6mbtllfOZHaUuwrrQYc6SLjY7ZNXFTFsjoagEVOdjeaU8MKLHvGum3cV7m7UVq8QMNi3KpRrLB7JRW8zG-GWZ1MRCRPrY9Otuevsb-jldQpJ6u-0fboYKHrJBIaQoCT0kEbtCS-hkgWeoCdezs-J94tJ7X5qNpjNrGl3aL6Z1xu01QH0DqpPKUatVPFleGcaaJmNWASHZWxN1_YZ6ZdlsYwFeiOm7MRGp8eY_LL0zDhJXEye0ridb0FG_PTbea_ztEbrcEulDffZWMhW4mJjkvDvrgwAvbkOMVfZJUwjbEsxp58KfRaJ6LTdIMSy69UNDl0k7HqF_v1Un84HchhZUmd_8nNV_EezMmV9an1wmzzjmQ0PhoZP0JQ05u96s7ciJo6mt0m8kz9YuYrHL0SN1HMA_E" -- cspell:disable-line
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

albertCreateApiKey :: ApiKeyCreateDTO
albertCreateApiKey =
  ApiKeyCreateDTO {name = albertApiKey.name, expiresAt = dt' 2052 1 21}

myAppKeyCreate :: AppKeyCreateDTO
myAppKeyCreate =
  AppKeyCreateDTO {name = myAppKey.name}

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
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

nikolaToken :: UserToken
nikolaToken =
  UserToken
    { uuid = u' "240a04e9-3c8c-4631-8921-ccb1ae3ddfa3"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userNikola.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE4MDgxODU2OTEsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiIyNDBhMDRlOS0zYzhjLTQ2MzEtODkyMS1jY2IxYWUzZGRmYTMiLCJ1c2VyVXVpZCI6IjMwZDQ4Y2Y0LThjOGEtNDk2Zi1iYWZlLTU4NWJkMjM4Zjc5OCIsInZlcnNpb24iOjR9.clKmIZmKdOsrjF2L-niBa2aqIC5KajU4tI9A4SxfDv8Z26bdPzgJTjBfW9EQmDs9Vvm1pYJp2ejQSR3Z2yQ4k2KcKtFpAkLXO0njdW1Z3sCopXQmeOpoU1ATngmpyKy95Imgnm-UrELG_qilvuU8OcBnQVXwdA0yzIzJB_Ye58MjeId4F6oOFdNXN3rR9PpCTLoTy3w3rm5-qEnAlq0_z10Kbk4ZtsA53FxYKxDa-5GJ7tu77wMSPmF1vWBq-jqiJVk25KyJ3CCVG1cnARQpqljsm0e4xhg6aPiQQlJ8SwiR9TwXtbFPt8nwo9dgOp2pFjZFpg4kRt4D9FgWg_mVQ1DN4ObLVWttUEQCrE1liSR1gTVpFy_ApwPcOe18BaqA_oXwvbycJOy92FZweY1q2gak6qzgvFXtS-iRa9D-CFnyW84-AvDluclvPwlHL8HrFlmDGD_qJdzZbWRuv-vxCkNw6BBy0NRxDUg8TVuE3lGe-_3pIkt3-oLjgWi_kzEbdLYEpzun3q4Zlf92rCF8YXMoEol7pVT0Uih4AbVIYyEgtMR_MS8cFCq2Tqa-CJxRjqv0skN96iaArl-OWn1HOu8BDa3TKYePf715MppdUpOV73zW4lbhQBlUbAp0PyJrWRwjDACQFFVUslY7B-F-Xhf2uYw-xIxUxy8R37V7Iqc" -- cspell:disable-line
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

nikolaCreateToken :: LoginDTO
nikolaCreateToken =
  LoginDTO {email = userNikola.email, password = "password", code = Nothing}

isaacToken :: UserToken
isaacToken =
  UserToken
    { uuid = u' "a352fdfe-70ff-4ba1-a98e-4d5cf0c5da4e"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userIsaac.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE4MDgxODU2MTAsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiJhMzUyZmRmZS03MGZmLTRiYTEtYTk4ZS00ZDVjZjBjNWRhNGUiLCJ1c2VyVXVpZCI6ImUxYzU4ZTUyLTA4MjQtNDUyNi04ZWJlLWVjMzhlZWM2NzAzMCIsInZlcnNpb24iOjR9.ALICF-tWzUevJQNKfRhWMUY-Bk0_OJ75g6tCs0gY_5mY4p-d-s2_gUDMMXjyqKb5gJWTuoeAKa9rrlzsKFDLiyEeVSortB7UwYHBWTlroVx_6yDg06XRMCgCnPwjrZDeHh3udEcUBU1FCZxoZG-lo0tPZE0U6MXkQcvsWWOteUP4TQz2sDHpbSCe4EVabMiKGNbXhuHg2uCvzltUST9kKlqprGd7wE75mqAV2ifXzPf466rCSuSxUp41322ymrTMCM3u4MHF_tbA6ryG5GJ_jGYLzKR9fnA_o5dR_LDzs2EP24MXKCk1l--vZe-EdOuiCtZ_7w5W3rr0QCD2jPeqr7y_kOSzzy1UJreVqZWUrlk8Qtkb4uOofZ23YtxN_bObzIsDP4d6twdfSOyzyzgUgQ9KQ4Wqv1P8evR3_qoqWphPF49nQ8LMdEavxGyRCzPsnBIAe274DK58yN6bPuIWjQJwo5gT6R6rJ_Xx8R9xL0Xi_0e_ZORWeynZ4atgdl4LlANT485ja_1Z5VENmp-90WaPkMYZQi3QpA7VqXXYX-z-fjdf-xesZKmP-ZdPPQaZjzV12h9m6rvTASvkuF4bKasnqdd-gzKl3iuFfzrxTE-qAKv-2W8MK31VITdFS-SrqteUhVhbYo8vBHGt-J8oq3qjA2AeZHogObrjIV43CoU" -- cspell:disable-line
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

isaacCreateToken :: LoginDTO
isaacCreateToken =
  LoginDTO {email = userIsaac.email, password = "password", code = Nothing}
