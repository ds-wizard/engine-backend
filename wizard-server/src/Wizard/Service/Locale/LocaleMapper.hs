module Wizard.Service.Locale.LocaleMapper where

import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Locale.Model.Locale.Locale
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Model.Locale.LocaleList
import Wizard.Model.Registry.RegistryLocale
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Service.Locale.LocaleUtil
import WizardLib.Common.Util.Coordinate

toDTO :: Bool -> LocaleList -> LocaleDTO
toDTO registryEnabled locale =
  LocaleDTO
    { lId = locale.lId
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , organizationId = locale.organizationId
    , localeId = locale.localeId
    , version = locale.version
    , defaultLocale = locale.defaultLocale
    , enabled = locale.enabled
    , remoteLatestVersion =
        if registryEnabled
          then locale.remoteVersion
          else Nothing
    , organization =
        case (registryEnabled, locale.remoteOrganizationName) of
          (True, Just orgName) ->
            Just $
              OrganizationSimple
                { organizationId = locale.organizationId
                , name = orgName
                , logo = locale.remoteOrganizationLogo
                }
          _ -> Nothing
    , createdAt = locale.createdAt
    , updatedAt = locale.updatedAt
    }

toDetailDTO :: Locale -> Bool -> [RegistryLocale] -> [RegistryOrganization] -> [String] -> Maybe String -> LocaleDetailDTO
toDetailDTO locale registryEnabled localeRs orgRs versionLs registryLink =
  LocaleDetailDTO
    { lId = locale.lId
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , organizationId = locale.organizationId
    , localeId = locale.localeId
    , version = locale.version
    , defaultLocale = locale.defaultLocale
    , license = locale.license
    , readme = locale.readme
    , recommendedAppVersion = locale.recommendedAppVersion
    , enabled = locale.enabled
    , versions = L.sort versionLs
    , remoteLatestVersion =
        case (registryEnabled, selectLocaleByOrgIdAndLocaleId locale localeRs) of
          (True, Just localeR) -> Just $ localeR.remoteVersion
          _ -> Nothing
    , registryLink =
        if registryEnabled
          then registryLink
          else Nothing
    , organization =
        if registryEnabled
          then selectOrganizationByOrgId locale orgRs
          else Nothing
    , createdAt = locale.createdAt
    , updatedAt = locale.updatedAt
    }

toLocaleList :: Locale -> LocaleList
toLocaleList locale =
  LocaleList
    { lId = locale.lId
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , organizationId = locale.organizationId
    , localeId = locale.localeId
    , version = locale.version
    , defaultLocale = locale.defaultLocale
    , enabled = locale.enabled
    , remoteVersion = Nothing
    , remoteOrganizationName = Nothing
    , remoteOrganizationLogo = Nothing
    , createdAt = locale.createdAt
    , updatedAt = locale.updatedAt
    }

fromCreateDTO :: LocaleCreateDTO -> String -> Bool -> U.UUID -> UTCTime -> Locale
fromCreateDTO reqDto organizationId defaultLocale tenantUuid now =
  Locale
    { lId = buildCoordinate organizationId reqDto.localeId reqDto.version
    , name = reqDto.name
    , description = reqDto.description
    , code = reqDto.code
    , organizationId = organizationId
    , localeId = reqDto.localeId
    , version = reqDto.version
    , defaultLocale = defaultLocale
    , license = reqDto.license
    , readme = reqDto.readme
    , recommendedAppVersion = reqDto.recommendedAppVersion
    , enabled = False
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: Locale -> LocaleChangeDTO -> UTCTime -> Locale
fromChangeDTO locale reqDto now =
  Locale
    { lId = locale.lId
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , organizationId = locale.organizationId
    , localeId = locale.localeId
    , version = locale.version
    , defaultLocale = reqDto.defaultLocale
    , license = locale.license
    , readme = locale.readme
    , recommendedAppVersion = locale.recommendedAppVersion
    , enabled = reqDto.enabled
    , tenantUuid = locale.tenantUuid
    , createdAt = locale.createdAt
    , updatedAt = now
    }

buildLocaleUrl :: String -> Locale -> [RegistryLocale] -> Maybe String
buildLocaleUrl clientRegistryUrl locale localeRs =
  case selectLocaleByOrgIdAndLocaleId locale localeRs of
    Just localeR ->
      Just $
        clientRegistryUrl
          ++ "/locales/"
          ++ buildCoordinate localeR.organizationId localeR.localeId localeR.remoteVersion
    Nothing -> Nothing
