module Wizard.Integration.Http.Registry.RequestMapper where

import Data.ByteString.Char8 as BS
import Data.Map.Strict as M
import Servant
import Servant.Client
import Prelude hiding (lookup)

import qualified Registry.Api.Handler.DocumentTemplate.List_GET as TML_List_GET
import qualified Registry.Api.Handler.Locale.List_GET as LOC_List_GET
import Registry.Api.Handler.Organization.Detail_State_PUT
import Registry.Api.Handler.Organization.List_POST
import Registry.Api.Handler.Organization.List_Simple_GET
import qualified Registry.Api.Handler.Package.List_GET as PKG_List_GET
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationCreateJM ()
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Constant.Api
import Shared.Constant.DocumentTemplate
import Shared.Constant.KnowledgeModel
import Shared.Util.String (f', splitOn)
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Statistics.InstanceStatistics

toRetrieveOrganizationsRequest :: ClientM (Headers '[Header "x-trace-uuid" String] [OrganizationSimpleDTO])
toRetrieveOrganizationsRequest = client list_simple_GET_Api

toCreateOrganizationRequest
  :: ServerConfig
  -> OrganizationCreateDTO
  -> String
  -> ClientM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
toCreateOrganizationRequest serverConfig reqDto clientUrl = client list_POST_Api reqDto (Just clientUrl)

toConfirmOrganizationRegistrationRequest
  :: RegistryConfirmationDTO -> ClientM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
toConfirmOrganizationRegistrationRequest reqDto =
  client
    detail_state_PUT_Api
    (OrganizationStateDTO {active = True})
    reqDto.organizationId
    (Just reqDto.hash)

toRetrievePackagesRequest
  :: AppConfigRegistry -> InstanceStatistics -> ClientM (Headers '[Header "x-trace-uuid" String] [PackageSimpleDTO])
toRetrievePackagesRequest appConfig iStat =
  client
    PKG_List_GET.list_GET_Api
    mTokenHeader
    xUserCountHeaderName
    xPkgCountHeaderName
    xQtnCountHeaderName
    xBranchCountHeaderName
    xDocCountHeaderName
    xTmlCountHeaderName
    organizationId
    kmId
    metamodelVersion
  where
    mTokenHeader = Just $ "Bearer " ++ appConfig.token
    xUserCountHeaderName = Just . show $ iStat.userCount
    xPkgCountHeaderName = Just . show $ iStat.pkgCount
    xQtnCountHeaderName = Just . show $ iStat.qtnCount
    xBranchCountHeaderName = Just . show $ iStat.branchCount
    xDocCountHeaderName = Just . show $ iStat.docCount
    xTmlCountHeaderName = Just . show $ iStat.tmlCount
    organizationId = Nothing
    kmId = Nothing
    metamodelVersion = Just kmMetamodelVersion

toRetrieveTemplatesRequest
  :: AppConfigRegistry -> ClientM (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])
toRetrieveTemplatesRequest appConfig =
  client TML_List_GET.list_GET_Api mTokenHeader organizationId tmlId metamodelVersion
  where
    mTokenHeader = Just $ "Bearer " ++ appConfig.token
    organizationId = Nothing
    tmlId = Nothing
    metamodelVersion = Just documentTemplateMetamodelVersion

toRetrieveLocaleRequest :: String -> AppConfigRegistry -> ClientM (Headers '[Header "x-trace-uuid" String] [LocaleDTO])
toRetrieveLocaleRequest version appConfig =
  client LOC_List_GET.list_GET_Api mTokenHeader organizationId lclId recommendedAppVersion
  where
    mTokenHeader = Just $ "Bearer " ++ appConfig.token
    organizationId = Nothing
    lclId = Nothing
    recommendedAppVersion =
      case splitOn "." version of
        [major, minor, _] -> Just . f' "%s.%s.%s" $ [major, minor, "0"]
        _ -> Just "1.0.0"

toRetrievePackageBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrievePackageBundleByIdRequest serverConfig appConfig pkgId =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = serverConfig.url ++ "/packages/" ++ pkgId ++ "/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token)]
    , requestBody = BS.empty
    , multipartFileName = Nothing
    }

toRetrieveTemplateBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrieveTemplateBundleByIdRequest serverConfig appConfig tmlId =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = serverConfig.url ++ "/document-templates/" ++ tmlId ++ "/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token)]
    , requestBody = BS.empty
    , multipartFileName = Nothing
    }

toRetrieveLocaleBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrieveLocaleBundleByIdRequest serverConfig appConfig lclId =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = serverConfig.url ++ "/locales/" ++ lclId ++ "/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token)]
    , requestBody = BS.empty
    , multipartFileName = Nothing
    }
