module WizardLib.Public.Api.Resource.User.Group.UserGroupDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailDTO
import WizardLib.Public.Api.Resource.User.UserWithMembershipJM ()

instance FromJSON UserGroupDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupDetailDTO where
  toJSON = genericToJSON jsonOptions
