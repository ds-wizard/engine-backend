module Wizard.Integration.Http.Registry.ResponseMapper where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Model.Error.Error
import Wizard.Integration.Http.Common.ResponseMapper
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Integration.Resource.Package.PackageSimpleIJM ()

toRetrieveOrganizationsResponse :: Response BSL.ByteString -> Either AppError [OrganizationSimpleDTO]
toRetrieveOrganizationsResponse = deserializeResponseBody

toCreateOrganizationResponse :: Response BSL.ByteString -> Either AppError OrganizationDTO
toCreateOrganizationResponse = deserializeResponseBody

toConfirmOrganizationRegistrationResponse :: Response BSL.ByteString -> Either AppError OrganizationDTO
toConfirmOrganizationRegistrationResponse = deserializeResponseBody

toRetrievePackagesResponse :: Response BSL.ByteString -> Either AppError [PackageSimpleIDTO]
toRetrievePackagesResponse = deserializeResponseBody

toRetrievePackageBundleByIdResponse :: Response BSL.ByteString -> Either AppError BSL.ByteString
toRetrievePackageBundleByIdResponse response = Right . getResponseBody $ response
