module Registry.Api.Resource.UserEmailLink.UserEmailLinkSM where

import Data.Swagger

import Registry.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Registry.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Registry.Model.UserEmailLink.UserEmailLinkType
import Shared.Common.Util.Swagger
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkJM ()

instance ToSchema UserEmailLinkType

instance ToSchema (UserEmailLinkDTO UserEmailLinkType) where
  declareNamedSchema = toSwagger forgottenTokenUserEmailLinkDto
