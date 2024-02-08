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
    { uuid = u' "43d44c6a-d22e-4954-bee6-0fedc8c990df"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userAlbert.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzU4ODQyOTAsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI0M2Q0NGM2YS1kMjJlLTQ5NTQtYmVlNi0wZmVkYzhjOTkwZGYiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjR9.I39ofNurjfjDgeCgh3zFfrK6tC4KEQSOnxmvxY8lWFoO1szsKqKzWXQPfAnJzZF6tw6hS2Fj3p6xtvjHT-dzMcfP8DDzTO8bUua0l86u1jsVyNoqzM1V1SWcvfALah7_6WFcO9izXKvYV1VyL8ouTkaiTNxc365qeKFru6_wH5ts9vz3wcwvJaimeUEsacEacZXpmZGhs8zjVhzw9fluTyu_gOrx-58ci2JQJfQbP0JBfGhfhIxaVqBR25E-U8Zn1Tut-2mYHIcHx99iO6mbtllfOZHaUuwrrQYc6SLjY7ZNXFTFsjoagEVOdjeaU8MKLHvGum3cV7m7UVq8QMNi3KpRrLB7JRW8zG-GWZ1MRCRPrY9Otuevsb-jldQpJ6u-0fboYKHrJBIaQoCT0kEbtCS-hkgWeoCdezs-J94tJ7X5qNpjNrGl3aL6Z1xu01QH0DqpPKUatVPFleGcaaJmNWASHZWxN1_YZ6ZdlsYwFeiOm7MRGp8eY_LL0zDhJXEye0ridb0FG_PTbea_ztEbrcEulDffZWMhW4mJjkvDvrgwAvbkOMVfZJUwjbEsxp58KfRaJ6LTdIMSy69UNDl0k7HqF_v1Un84HchhZUmd_8nNV_EezMmV9an1wmzzjmQ0PhoZP0JQ05u96s7ciJo6mt0m8kz9YuYrHL0SN1HMA_E"
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
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzU4ODQyOTAsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI0M2Q0NGM2YS1kMjJlLTQ5NTQtYmVlNi0wZmVkYzhjOTkwZGYiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjR9.I39ofNurjfjDgeCgh3zFfrK6tC4KEQSOnxmvxY8lWFoO1szsKqKzWXQPfAnJzZF6tw6hS2Fj3p6xtvjHT-dzMcfP8DDzTO8bUua0l86u1jsVyNoqzM1V1SWcvfALah7_6WFcO9izXKvYV1VyL8ouTkaiTNxc365qeKFru6_wH5ts9vz3wcwvJaimeUEsacEacZXpmZGhs8zjVhzw9fluTyu_gOrx-58ci2JQJfQbP0JBfGhfhIxaVqBR25E-U8Zn1Tut-2mYHIcHx99iO6mbtllfOZHaUuwrrQYc6SLjY7ZNXFTFsjoagEVOdjeaU8MKLHvGum3cV7m7UVq8QMNi3KpRrLB7JRW8zG-GWZ1MRCRPrY9Otuevsb-jldQpJ6u-0fboYKHrJBIaQoCT0kEbtCS-hkgWeoCdezs-J94tJ7X5qNpjNrGl3aL6Z1xu01QH0DqpPKUatVPFleGcaaJmNWASHZWxN1_YZ6ZdlsYwFeiOm7MRGp8eY_LL0zDhJXEye0ridb0FG_PTbea_ztEbrcEulDffZWMhW4mJjkvDvrgwAvbkOMVfZJUwjbEsxp58KfRaJ6LTdIMSy69UNDl0k7HqF_v1Un84HchhZUmd_8nNV_EezMmV9an1wmzzjmQ0PhoZP0JQ05u96s7ciJo6mt0m8kz9YuYrHL0SN1HMA_E"
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
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzU4ODQyOTAsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI0M2Q0NGM2YS1kMjJlLTQ5NTQtYmVlNi0wZmVkYzhjOTkwZGYiLCJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInZlcnNpb24iOjR9.I39ofNurjfjDgeCgh3zFfrK6tC4KEQSOnxmvxY8lWFoO1szsKqKzWXQPfAnJzZF6tw6hS2Fj3p6xtvjHT-dzMcfP8DDzTO8bUua0l86u1jsVyNoqzM1V1SWcvfALah7_6WFcO9izXKvYV1VyL8ouTkaiTNxc365qeKFru6_wH5ts9vz3wcwvJaimeUEsacEacZXpmZGhs8zjVhzw9fluTyu_gOrx-58ci2JQJfQbP0JBfGhfhIxaVqBR25E-U8Zn1Tut-2mYHIcHx99iO6mbtllfOZHaUuwrrQYc6SLjY7ZNXFTFsjoagEVOdjeaU8MKLHvGum3cV7m7UVq8QMNi3KpRrLB7JRW8zG-GWZ1MRCRPrY9Otuevsb-jldQpJ6u-0fboYKHrJBIaQoCT0kEbtCS-hkgWeoCdezs-J94tJ7X5qNpjNrGl3aL6Z1xu01QH0DqpPKUatVPFleGcaaJmNWASHZWxN1_YZ6ZdlsYwFeiOm7MRGp8eY_LL0zDhJXEye0ridb0FG_PTbea_ztEbrcEulDffZWMhW4mJjkvDvrgwAvbkOMVfZJUwjbEsxp58KfRaJ6LTdIMSy69UNDl0k7HqF_v1Un84HchhZUmd_8nNV_EezMmV9an1wmzzjmQ0PhoZP0JQ05u96s7ciJo6mt0m8kz9YuYrHL0SN1HMA_E"
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
    { uuid = u' "82d27684-d6a2-4d25-a736-11110f1094ef"
    , name = "Token"
    , tType = LoginUserTokenType
    , userUuid = userNikola.uuid
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzU4ODQyMzksInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiI4MmQyNzY4NC1kNmEyLTRkMjUtYTczNi0xMTExMGYxMDk0ZWYiLCJ1c2VyVXVpZCI6IjMwZDQ4Y2Y0LThjOGEtNDk2Zi1iYWZlLTU4NWJkMjM4Zjc5OCIsInZlcnNpb24iOjR9.h8URlC98RayY9RmrPXNbP-P_mHBeLVD4qn-FqcApmCg0JZ7yQGIYUxS-KKrcjpVVw3iPq_rtXNtuHnrEqo6SNtH3zxArd9Eu0yMT9SDQE0UKRtc-czKu_7O1ZOg4ESphg5rZ1-mRxqPQnqnmnWAfauvpfVzOI8ctR_yTZ5lwxJ1h1loPb2ic1um8S727rFhYKHy7_E4wjSKV-WHIh5tMyx4M_7JsFIPrhLmPAIyULWgbvpzV9kwFeQP5l7NHxhdun2IQBEZGrNQRDyhsrScvsi9Z_cAqj4NXPf_f6qJRJy3nhOdZWoAVN4WAPhLA-j6Z2rCL1klu5q9h058hQ5kevJ13lFQ2dr3h9RFjHi42xCuziSnhgcIIYGjSii1gyURBNbnNVz3JqjmMPQ-ZTuUYenJlhTVcg2-iDqjy7LWqJ_y6nZeDjiPweofV6NAr1ZhWGspEjXRU-A2JzXUmZWM1ca3n3xgdIT8PWtVTcINBGaJS-p_ZG7-UVwmdsJsHMCPGEaPe25SQhoX18Ed7WbXivj3T928y7D9qg8Hh-n-dA1zk5RKjEXuArRfy2Zy-At4dXwp6bDzgLrj4TIfoKty5s78FFFMTDiudRnYjp9OmuuQoKvwk7-8il3HTM7QsJ1q2ZWW5Br8BNgevPAzuSCGWGuKFB7Eg_1C3waCaa8jtw0Q"
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
    , value = "eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE3MzU4ODQxNTYsInRlbmFudFV1aWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtMDAwMC0wMDAwMDAwMDAwMDAiLCJ0b2tlblV1aWQiOiIzNzVjOGY2Mi0yM2Q1LTRkNDgtOTdhZi1jMTNmM2UxMmE3NTMiLCJ1c2VyVXVpZCI6ImUxYzU4ZTUyLTA4MjQtNDUyNi04ZWJlLWVjMzhlZWM2NzAzMCIsInZlcnNpb24iOjR9.2JOxUk56morspsoQQedYPdZ9jK5KAa4Mftj1ZSfvsfhBzxyr--aWK-kirX_m8rkJSn8pjFY476qD13ulQWyO7b_YzlhacCrRpoezkqUmzH-a9vI1WqyeApXAsFCLF0XAFke7Yg9HzzS4QQ5MIKQPGyTUi_-VSGtM1xvX1g1IOXZpHs5k5Re2VH76IXt0RXjOXb0LyhYJNFf9uVSKhgjmNb76dTKY5d2fr-Q3Ztjns_C0zpMzZykTvGOI-J3JFqa9if_E6zXwb9GL-_dpjR1yPw36NL8GxKvFHsogPFUUBUc6-Or2Wk8G6o3ifKXzl78MN2sKJwc6iwxxaITq3gB435kWG-yijhmNlPynjzer9o2XXNqUB0Kg2PtV_U9V_VIwB0RGGM_Le1YKSXxvroftQaBP8O62zL_o0bHkH6Ip6rVCotUpO4r3V4aUpGTsy_MhjrUxO5P8To1Ac6FX31petgtIquQKdMAcAeZrkc6NeEEEXtsTrxVE1QnWBsszhao1NUCh9XuRIhtpOirUoJ0mP8RZCDTrIEEDzZXbPRm-D-A_BhH1NV7uuVMdHPz8EtplKmAZ-PQjlnXG8awtz4PO77KDW1amLQHjvhF2WyQvzRdXndr-EOOu3ppSBvwiTmx0hJHXhS2o5eGmuOblKYnZt-GtPXJKqTj4O4Zrpxm7vzg"
    , userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
    , sessionState = Nothing
    , expiresAt = dt' 2052 1 21
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2022 1 21
    }

isaacCreateToken :: LoginDTO
isaacCreateToken =
  LoginDTO {email = userIsaac.email, password = "password", code = Nothing}
