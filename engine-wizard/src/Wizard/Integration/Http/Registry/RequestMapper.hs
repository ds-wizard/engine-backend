module Wizard.Integration.Http.Registry.RequestMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict as M
import Prelude hiding (lookup)

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationCreateJM ()
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Shared.Constant.Api
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Statistics.InstanceStatistics

toRetrieveOrganizationsRequest :: ServerConfigRegistry -> AppConfigRegistry -> HttpRequest
toRetrieveOrganizationsRequest serverConfig appConfig =
  HttpRequest
    { _httpRequestRequestMethod = "GET"
    , _httpRequestRequestUrl = serverConfig ^. url ++ "/organizations/simple"
    , _httpRequestRequestHeaders = M.empty
    , _httpRequestRequestBody = BS.empty
    , _httpRequestMultipartFileName = Nothing
    }

toCreateOrganizationRequest :: ServerConfig -> OrganizationCreateDTO -> HttpRequest
toCreateOrganizationRequest serverConfig reqDto =
  HttpRequest
    { _httpRequestRequestMethod = "POST"
    , _httpRequestRequestUrl =
        serverConfig ^. registry . url ++ "/organizations?callback=" ++ serverConfig ^. general . clientUrl
    , _httpRequestRequestHeaders = M.fromList [("Content-Type", "application/json")]
    , _httpRequestRequestBody = BSL.toStrict . encode $ reqDto
    , _httpRequestMultipartFileName = Nothing
    }

toConfirmOrganizationRegistrationRequest :: ServerConfig -> RegistryConfirmationDTO -> HttpRequest
toConfirmOrganizationRegistrationRequest serverConfig reqDto =
  HttpRequest
    { _httpRequestRequestMethod = "PUT"
    , _httpRequestRequestUrl =
        serverConfig ^. registry . url ++ "/organizations/" ++ reqDto ^. organizationId ++ "/state?hash=" ++ reqDto ^.
        hash
    , _httpRequestRequestHeaders = M.fromList [("Content-Type", "application/json")]
    , _httpRequestRequestBody = BSL.toStrict . encode $ OrganizationStateDTO {_organizationStateDTOActive = True}
    , _httpRequestMultipartFileName = Nothing
    }

toRetrievePackagesRequest :: ServerConfigRegistry -> AppConfigRegistry -> InstanceStatistics -> HttpRequest
toRetrievePackagesRequest serverConfig appConfig iStat =
  HttpRequest
    { _httpRequestRequestMethod = "GET"
    , _httpRequestRequestUrl = serverConfig ^. url ++ "/packages"
    , _httpRequestRequestHeaders =
        M.fromList
          [ (authorizationHeaderName, "Bearer " ++ appConfig ^. token)
          , (xUserCountHeaderName, show $ iStat ^. userCount)
          , (xPkgCountHeaderName, show $ iStat ^. pkgCount)
          , (xQtnCountHeaderName, show $ iStat ^. qtnCount)
          ]
    , _httpRequestRequestBody = BS.empty
    , _httpRequestMultipartFileName = Nothing
    }

toRetrievePackageBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrievePackageBundleByIdRequest serverConfig appConfig pkgId =
  HttpRequest
    { _httpRequestRequestMethod = "GET"
    , _httpRequestRequestUrl = serverConfig ^. url ++ "/packages/" ++ pkgId ++ "/bundle"
    , _httpRequestRequestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig ^. token)]
    , _httpRequestRequestBody = BS.empty
    , _httpRequestMultipartFileName = Nothing
    }
