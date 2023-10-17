module Wizard.Api.Resource.User.OnlineUserInfoSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.OnlineUserInfoJM ()
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.OnlineUserInfo

instance ToSchema OnlineUserInfo where
  declareNamedSchema = toSwaggerWithType "type" userAlbertOnlineInfo
