module Wizard.Integration.Http.Registry.RequestMapper where

import Control.Lens ((^.))
import Data.ByteString.Char8 as BS
import Data.Map.Strict as M
import Prelude hiding (lookup)
import Servant
import Servant.Client

import LensesConfig
import Registry.Api.Handler.Organization.Detail_State_PUT
import Registry.Api.Handler.Organization.List_POST
import Registry.Api.Handler.Organization.List_Simple_GET
import Registry.Api.Handler.Package.List_GET
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationCreateJM ()
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Constant.Api
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Statistics.InstanceStatistics

toRetrieveOrganizationsRequest :: ClientM (Headers '[ Header "x-trace-uuid" String] [OrganizationSimpleDTO])
toRetrieveOrganizationsRequest = client list_simple_GET_Api

toCreateOrganizationRequest ::
     ServerConfig -> OrganizationCreateDTO -> ClientM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
toCreateOrganizationRequest serverConfig reqDto =
  client list_POST_Api reqDto (Just $ serverConfig ^. general . clientUrl)

toConfirmOrganizationRegistrationRequest ::
     RegistryConfirmationDTO -> ClientM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
toConfirmOrganizationRegistrationRequest reqDto =
  client
    detail_state_PUT_Api
    (OrganizationStateDTO {_organizationStateDTOActive = True})
    (reqDto ^. organizationId)
    (Just $ reqDto ^. hash)

toRetrievePackagesRequest ::
     AppConfigRegistry -> InstanceStatistics -> ClientM (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])
toRetrievePackagesRequest appConfig iStat =
  client list_GET_Api mTokenHeader xUserCountHeaderName xPkgCountHeaderName xQtnCountHeaderName organizationId kmId
  where
    mTokenHeader = Just $ "Bearer " ++ (appConfig ^. token)
    xUserCountHeaderName = Just . show $ iStat ^. userCount
    xPkgCountHeaderName = Just . show $ iStat ^. pkgCount
    xQtnCountHeaderName = Just . show $ iStat ^. qtnCount
    organizationId = Nothing
    kmId = Nothing

toRetrievePackageBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrievePackageBundleByIdRequest serverConfig appConfig pkgId =
  HttpRequest
    { _httpRequestRequestMethod = "GET"
    , _httpRequestRequestUrl = serverConfig ^. url ++ "/packages/" ++ pkgId ++ "/bundle"
    , _httpRequestRequestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig ^. token)]
    , _httpRequestRequestBody = BS.empty
    , _httpRequestMultipartFileName = Nothing
    }
