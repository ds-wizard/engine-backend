module Registry.Service.Locale.LocaleMapper where

import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Api.Resource.Locale.LocaleDetailDTO
import Registry.Model.Organization.Organization
import Registry.Service.Locale.LocaleUtil
import qualified Registry.Service.Organization.OrganizationMapper as OM_Mapper
import Shared.Model.Locale.Locale

toDTO :: [Organization] -> Locale -> LocaleDTO
toDTO orgs locale =
  LocaleDTO
    { lId = locale.lId
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , organizationId = locale.organizationId
    , localeId = locale.localeId
    , version = locale.version
    , organization = fmap OM_Mapper.toSimpleDTO . selectOrganizationByOrgId locale $ orgs
    , createdAt = locale.createdAt
    }

toDetailDTO :: Locale -> [String] -> Organization -> LocaleDetailDTO
toDetailDTO locale versions org =
  LocaleDetailDTO
    { lId = locale.lId
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , organizationId = locale.organizationId
    , localeId = locale.localeId
    , version = locale.version
    , license = locale.license
    , readme = locale.readme
    , recommendedAppVersion = locale.recommendedAppVersion
    , versions = versions
    , organization = OM_Mapper.toSimpleDTO org
    , createdAt = locale.createdAt
    }
