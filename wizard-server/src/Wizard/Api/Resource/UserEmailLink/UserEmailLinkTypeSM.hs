module Wizard.Api.Resource.UserEmailLink.UserEmailLinkTypeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Wizard.Api.Resource.UserEmailLink.UserEmailLinkTypeJM ()
import Wizard.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Wizard.Model.UserEmailLink.UserEmailLinkType

instance ToSchema UserEmailLinkType

instance ToSchema (UserEmailLinkDTO UserEmailLinkType) where
  declareNamedSchema = toSwagger forgottenPasswordUserEmailLinkDto
