module Wizard.Database.Migration.Development.User.Data.UserTokens where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User
import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.UserToken.UserTokenMapper

albertToken :: UserToken
albertToken =
  UserToken
    { uuid = u' "43d44c6a-d22e-4954-bee6-0fedc8c990df"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userAlbert.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzMxMTI1NTIsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI0M2Q0NGM2YS1kMjJlLTQ5NTQtYmVlNi0wZmVkYzhjOTkwZGYiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjN9.RDwmW_maHKmpSwt6ls6aEO_SqVMy1aLqNOgZWIzRYZI-b9AlVl8W71GBoXDzvSPFMuXWdfI4mjwD_ao8NEFbgUxaijW6Cms5P2YVLzMB2AmL0fOGWHp4PB-Q1lmWoX-yAhgQeqo1dV9H95v7UoLne4lnBtp6NQez1DtjwwFOrXajLZAWr68GUn973saUAI62rdrKJEG34uo7l8VBQIXx2s8C8mrvgdXgwUmcJ3zWWORhbVX-c69oTEBKmx_L7bc3SXeZUK0PUKZg9o20kMwQB9QrMGgfqXrYgWwxPexkvenzx0XHBb6d0hcjn-oFAeIL1zWohVRL03xQL0cywUulw2IUjtuhX6KXY7aFMwRaVaPtIDKN1nsEifZBlYRe35Vm0m8lr79575I3W9k8xvMGzgV3D_V9dGIMDRmhwFP_7wzyZd7ZOKgwGBf1EWJZKx5H_ZdTLfNQDgQ3DVZVRFlxUSDfRLJI2A0sZXsKa3_a-ELQ_DMM3fO-T4V7jXcC6mN9D7e3zzEWgesrFg3YtE81hUL3bwXxOI3N2loZJN03PYC1vPrcEvulf-jZaNFvaE6pN8Pj4VjML-T1VfEwXyyeHCUlZS9HfQ2CZUjuinmne4Ueqwjl5dW9rpDpvniZ8ytMGDPv47tmHjmT4rYHPeu5SlKjFbgOCjSarvxNU0c_8-E"
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
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzMxMTI1NTIsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI0M2Q0NGM2YS1kMjJlLTQ5NTQtYmVlNi0wZmVkYzhjOTkwZGYiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjN9.RDwmW_maHKmpSwt6ls6aEO_SqVMy1aLqNOgZWIzRYZI-b9AlVl8W71GBoXDzvSPFMuXWdfI4mjwD_ao8NEFbgUxaijW6Cms5P2YVLzMB2AmL0fOGWHp4PB-Q1lmWoX-yAhgQeqo1dV9H95v7UoLne4lnBtp6NQez1DtjwwFOrXajLZAWr68GUn973saUAI62rdrKJEG34uo7l8VBQIXx2s8C8mrvgdXgwUmcJ3zWWORhbVX-c69oTEBKmx_L7bc3SXeZUK0PUKZg9o20kMwQB9QrMGgfqXrYgWwxPexkvenzx0XHBb6d0hcjn-oFAeIL1zWohVRL03xQL0cywUulw2IUjtuhX6KXY7aFMwRaVaPtIDKN1nsEifZBlYRe35Vm0m8lr79575I3W9k8xvMGzgV3D_V9dGIMDRmhwFP_7wzyZd7ZOKgwGBf1EWJZKx5H_ZdTLfNQDgQ3DVZVRFlxUSDfRLJI2A0sZXsKa3_a-ELQ_DMM3fO-T4V7jXcC6mN9D7e3zzEWgesrFg3YtE81hUL3bwXxOI3N2loZJN03PYC1vPrcEvulf-jZaNFvaE6pN8Pj4VjML-T1VfEwXyyeHCUlZS9HfQ2CZUjuinmne4Ueqwjl5dW9rpDpvniZ8ytMGDPv47tmHjmT4rYHPeu5SlKjFbgOCjSarvxNU0c_8-E"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
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
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

nikolaToken :: UserToken
nikolaToken =
  UserToken
    { uuid = u' "82d27684-d6a2-4d25-a736-11110f1094ef"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userNikola.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzMxMTI3ODYsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI4MmQyNzY4NC1kNmEyLTRkMjUtYTczNi0xMTExMGYxMDk0ZWYiLCJ1c2VyVXVpZCI6IjMwZDQ4Y2Y0LThjOGEtNDk2Zi1iYWZlLTU4NWJkMjM4Zjc5OCIsInZlcnNpb24iOjN9.3i-KeXg1Hzfn14cwUCihaF8ojbNnscGKGtaC8sJzbAgy98ihVkQ1H7eDE5XTOZiRuBt2cE-Q3HgK6trgE-0jPfhpQhyV3zdUwf3yKJ8LdJQGT6nhb6zvMHTUFO5BLfS7bK_SPMFx086ctALqRPKQjh6JPBFGHZ2tWnyfHJLFS1Fxo15zNPcsY3OVx7AAlEdf-DEnRrg_Sobavr54cD1aaTwICHtat16PvQRiGM0VGHClLEPicHMXcAQoPS_-ikBsaRIHyJYxuaL11V6NTWWsGz5nwzOCKLMKCDotR-ZF-khv0Ep0g6tiI1QFqF5hvfjAog3zszfwMGAbijdXH30fKMFqhSbdox_6wHrNTtuTTTrScGjdMrkcmWC-HK73bFsY93B3-qb6kWz_6CIP1nsHI2h840AAg-sVOJaldFoRj3c2UdpICPbpzgOs7dI-P15HVxBUEMekkzFqTdsuIywTxPdHgwp57HrD3N9Mxe_WfmOnKy-Thze7ITYDTMckaKWxb727hrA-G1gUMQOE5uQmeXrh33dTJgDwdWjJi9-Wb2iEXmrlHFQSMKjCc5b7uUJlZOVwEW4lLOZjOgru4oubH9IIvYw6USSWbTm0sKWEeTgjOImWmwD5R5jL6Hn6zAFPh31Pc44Sv9p304QtYU1w6a6Te2OTwf4bqqkXws_xg40"
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
    { uuid = u' "375c8f62-23d5-4d48-97af-c13f3e12a753"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userIsaac.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzMxMTI4MzgsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiIzNzVjOGY2Mi0yM2Q1LTRkNDgtOTdhZi1jMTNmM2UxMmE3NTMiLCJ1c2VyVXVpZCI6ImUxYzU4ZTUyLTA4MjQtNDUyNi04ZWJlLWVjMzhlZWM2NzAzMCIsInZlcnNpb24iOjN9.tJHNahKlvcZsq8vi1hnZYHvmY-JPYIUAutVyZgAZ6S-LePQ70BG_d1fGzcFGEEBCXOKTqqduFxj1ONTjMRgGZZak0x9inm1rw3W1dXv3Je3W5XlmVEwbM4A5nryRN2bqiP3zyfjV5UpEFa8cDokrWq3Hn_eFrTDBErXANPdZGfg_diZ5sqdn1BERYgN3LPkC_oWl9yJDFm3oqrBHs5cOcJvr4_lvVnrtgq-O9_dsPFjA828kwuNgQAzvC8NpXAw_U76v4cGCm3QgZ-hLzvg_ocyTDCvJG1m4w1uMWS15rOMsVM4tBykGxlrPQYB5WkaOK8HjZmJZKzMxbIwPYSwyoLYpalWgbFYKsWMfpRVNUqtGhgb9YUtxOknmT3yzZVkNMYBM8K5P4Q9C158IUZQsdN2lgCYby-BuEXBN3Vecxd4sbkn4E2vT7GXmGhooUlFcCNjOXx6cuub4RtGg9guGQKsKUeH8HLJtLficJiACi7sk_PZmp6Ffidae3B11qktbAX2cWSkntEnUB2wepPXFKPeZwmK8w-grVGQeh_9dCwSNWxGzkRrwB7ilEgLd2LLkxNoRmDq-cwb11gILuBFIQrnzuZ7ZDzOO2uSZhu-TumTYS5crLOyCq6968MJ74vPYYTTPIutnXnQGvrYSpbTZSCfxIwv5szUUBvoKpxxThZQ"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

isaacCreateToken :: LoginDTO
isaacCreateToken =
  LoginDTO {email = userIsaac.email, password = "password", code = Nothing}
