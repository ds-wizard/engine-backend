module Wizard.Integration.Http.Registry.RequestMapper where

import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict as M
import Servant
import Servant.Client
import Prelude hiding (lookup)

import qualified RegistryLib.Api.Handler.DocumentTemplate.List_GET as TML_List_GET
import qualified RegistryLib.Api.Handler.Locale.List_GET as LOC_List_GET
import RegistryLib.Api.Handler.Organization.Detail_State_PUT
import RegistryLib.Api.Handler.Organization.List_POST as ORG_List_POST
import RegistryLib.Api.Handler.Organization.List_Simple_GET
import qualified RegistryLib.Api.Handler.Package.List_Bundle_POST as PKG_List_Bundle_POST
import qualified RegistryLib.Api.Handler.Package.List_GET as PKG_List_GET
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationStateDTO
import RegistryLib.Api.Resource.Organization.OrganizationStateJM ()
import RegistryLib.Api.Resource.Package.PackageSimpleDTO
import Shared.Common.Constant.Api
import Shared.Common.Model.Http.HttpRequest
import Shared.Common.Util.String (f', splitOn)
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Statistics.InstanceStatistics
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleJM ()
import WizardLib.KnowledgeModel.Constant.KnowledgeModel

toRetrieveOrganizationsRequest :: ClientM (Headers '[Header "x-trace-uuid" String] [OrganizationSimpleDTO])
toRetrieveOrganizationsRequest = client list_simple_GET_Api

toCreateOrganizationRequest
  :: ServerConfig
  -> OrganizationCreateDTO
  -> String
  -> ClientM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
toCreateOrganizationRequest serverConfig reqDto clientUrl = client list_POST_Api Nothing reqDto (Just clientUrl)

toConfirmOrganizationRegistrationRequest
  :: RegistryConfirmationDTO -> ClientM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
toConfirmOrganizationRegistrationRequest reqDto =
  client
    detail_state_PUT_Api
    (OrganizationStateDTO {active = True})
    reqDto.organizationId
    reqDto.hash

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
    , multipart = Nothing
    }

toRetrieveTemplateBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrieveTemplateBundleByIdRequest serverConfig appConfig tmlId =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = serverConfig.url ++ "/document-templates/" ++ tmlId ++ "/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token)]
    , requestBody = BS.empty
    , multipart = Nothing
    }

toRetrieveLocaleBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrieveLocaleBundleByIdRequest serverConfig appConfig lclId =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = serverConfig.url ++ "/locales/" ++ lclId ++ "/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token)]
    , requestBody = BS.empty
    , multipart = Nothing
    }

toUploadPackageBundleRequest :: AppConfigRegistry -> PackageBundleDTO -> ClientM (Headers '[Header "x-trace-uuid" String] PackageBundleDTO)
toUploadPackageBundleRequest appConfig =
  client PKG_List_Bundle_POST.list_bundle_POST_Api mTokenHeader
  where
    mTokenHeader = Just $ "Bearer " ++ appConfig.token

toUploadDocumentTemplateBundleRequest :: ServerConfigRegistry -> AppConfigRegistry -> BSL.ByteString -> HttpRequest
toUploadDocumentTemplateBundleRequest serverConfig appConfig bundle =
  HttpRequest
    { requestMethod = "POST"
    , requestUrl = serverConfig.url ++ "/document-templates/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token)]
    , requestBody = BSL.toStrict bundle
    , multipart = Just $ HttpRequestMultipart {key = "file", fileName = Just "file.zip", contentType = Just "application/zip"}
    }

toUploadLocaleBundleRequest :: ServerConfigRegistry -> AppConfigRegistry -> BSL.ByteString -> HttpRequest
toUploadLocaleBundleRequest serverConfig appConfig bundle =
  HttpRequest
    { requestMethod = "POST"
    , requestUrl = serverConfig.url ++ "/locales/bundle"
    , requestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token)]
    , requestBody = BSL.toStrict bundle
    , multipart = Just $ HttpRequestMultipart {key = "file", fileName = Just "file.zip", contentType = Just "application/zip"}
    }
