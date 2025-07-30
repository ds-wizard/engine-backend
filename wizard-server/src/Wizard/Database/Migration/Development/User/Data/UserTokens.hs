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
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3NzIxNTE2NzYsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI1NGVhMDcyZC1iNWY5LTQyNTEtYjNhNC1hZTE3NzM2MDUwOWMiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjR9.VRCclpdJZnvlVt3fLoHsyHeIKgcRP2ZKNOSH04pU_2LgnWCxoz7Ka3EAGhuDrkHBgzGJNSAAvXQ4NKeAYNp_CvRgNBRV1zab4oVB2xnyCIXuzOAR_Y2QQ2MTBsGiYhiPzwifqr1DoAXz9TGSqeEIwtkUQophBAmwvvTvD4u66WtejEllWPOcJYo6oODMIuhaa-tFpStr0SbnVjv6wRUsh6yqgcQ0NA_2_rbAQ7AwQP02FLrBGhTZJYWPmchqeH3KnZPUgbw9fq0gwwUVwhZ9m6UKz71jRjpYjk02nUc3qZK-R8FOmfwIqg70c6ymqWXNqp2n4UJnZPvXyOY9gzLcUPiiO0i1B8CkRqTcqH425BW-CCdAgYFEOCkiJlaDTDbeYMenetYz64gPSHVx9fezCFwp-hP7GN5TcIX5X59m82bSe2DrvL6fw3gMes34b22WyAa-w-sf0-FxRYLZ5eMzG0zdcQeDmN3W128o5cnVgMTntG779YX-L9ixz0YNt88wQgMzVyfZDXFQpLBSkR-aTSa-ZA__8r79oc9qBb10v1ChBAWyyUlzruzTOKe1h9sViAmGf-eUArkHXCvgnKSu-_40JxVs2DW0gdfX4ZY5gaUBuz38PsA9cgJue1pjDQCmp20UwwPaX8d-lS2GC8leDXNHhxT8yQbgBI-EqoHd-xk" -- cspell:disable-line
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
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3NzIxNTE0ODIsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiIyNDBhMDRlOS0zYzhjLTQ2MzEtODkyMS1jY2IxYWUzZGRmYTMiLCJ1c2VyVXVpZCI6IjMwZDQ4Y2Y0LThjOGEtNDk2Zi1iYWZlLTU4NWJkMjM4Zjc5OCIsInZlcnNpb24iOjR9.XuIEsqe7wyMY5TCPAyVjRgeInbqoBC0jFsbMfxZGhjofs5Fkmmw6wYMMUTnjBuCpReXdheJh3XIHDbtNCnAImkeIKdNKcQmagRgjZwIu-hvGMeGdRa5cC6O2Rycn_17LqqcHX4Oqz-xCXuoMK6zhrHH5InibRSfdWcPYuT3pVSwGFM1_CBA7FZxUjUoltVI7xPlGIHzYzmQsG_LM0_t_LuxRIq9hxKaGCEzM8f8R3jUgMW9dmcwiPF15rT1xCi_zlttd18MOOm5vogn_JssL2m9ckoOOz4LQKeE2KAiyEyb9ZJq_6enk7CiO02mkvWA1SvSNifcSV9zSY_rT1GAcmQxeMlQlx-9mD2cAZzcYq0guviyWlIlXCR8Ho6eHkGBMuubUIku8ZrNta-NRh-UUvfZ53H0LuBESVFNyJ8d0iVSmSIXYv0DHpWK4sCTOs4y7yFZEK6pMGXAJajg8z9bnZAYFhTkHAhnm30XOgt3qnt_0dy4B5XLv21g4-cfc_XjlSiqdPSAhh9eeznbzxpJCCEHLBUVWzgkRW9rbRPA49sgTAcIt36bZKjVsNqHtbiQEAg54ON4ZVQkRDMiVZKWVFIZIEBhRJxpy6bJ04V8p9WyPH1Losb9-dVUsFGfeDnDRErpCgteauWvpZWO6HnCiSZEiwCsIxXKpe61Zgk3atxM" -- cspell:disable-line
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
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3NzIxNTE1ODQsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiJhMzUyZmRmZS03MGZmLTRiYTEtYTk4ZS00ZDVjZjBjNWRhNGUiLCJ1c2VyVXVpZCI6ImUxYzU4ZTUyLTA4MjQtNDUyNi04ZWJlLWVjMzhlZWM2NzAzMCIsInZlcnNpb24iOjR9.2KKh8adCGkpoQxOsHIpxEwkAPXzXGo6s5rWqbije2YpafCY2a9-pUFGktQUTf88FL1rIvo6MKtKGag3FkG6s2jnazyIdQqCfqzmxfyo5KphRHywEHaHJ39PXtxsHspvE4nZ367K9uz6XHU6f7MLo5-3WiIqB1RSwelUDt1rBVzbOFo3TrP7FLlawmDMvbGTojiBq4VO98Y8X3c6TIq_6h807CNNumm7d1yOee1Nao7BCPbUokncAvRiDJ4qkUWhdYesYz0KgSbdgTqUPcy5IjO_une00IECI9H2Rl2k6wjxqgrkKqdI36I-kDDFsUeRmrEWSt-EPfkRNZjXbAxyms-4UfTnRwv-BFgTo5S_xGdPiuAYdODdNgLzO2qbjMaUbPPFO2qIBeiV1X54veIb5D6H7WY7YRJH7Z8NhHLT8_941cTctAmvwvYWsOEOdN1rSXbEbk-wC6-ODvgCW6HbCddbZr0tIRNF3BZTbi6CX3apl4OcUE-nSU494qD1FAwYWux-hlNy-r1nMjmDoB0BKMNHgRv6AjobnWjQFru9s4iXZ3tdqXcfPifS16FHd_OYWLQOyVf2W-vnW9AznstqyMDjn_jjCtvjGh2ClTtrlLypO_U0n_TLlHihyJgnQRstWOcUqZJb8SVcnXDDLeyoRwZtTv-0vexywcq-Iogw5870" -- cspell:disable-line
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

isaacCreateToken :: LoginDTO
isaacCreateToken =
  LoginDTO {email = userIsaac.email, password = "password", code = Nothing}
