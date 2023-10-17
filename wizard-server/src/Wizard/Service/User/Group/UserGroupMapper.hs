module Wizard.Service.User.Group.UserGroupMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.User.UserGroupSuggestion
import WizardLib.Public.Model.User.UserGroup

toSuggestion :: UserGroup -> UserGroupSuggestion
toSuggestion userGroup =
  UserGroupSuggestion
    { uuid = userGroup.uuid
    , name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    }

fromCreate :: U.UUID -> String -> Maybe String -> Bool -> U.UUID -> UTCTime -> UserGroup
fromCreate uuid name description private tenantUuid now =
  UserGroup
    { uuid = uuid
    , name = name
    , description = description
    , private = private
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChange :: UserGroup -> String -> Maybe String -> Bool -> UTCTime -> UserGroup
fromChange userGroup name description private now =
  UserGroup
    { uuid = userGroup.uuid
    , name = name
    , description = description
    , private = private
    , tenantUuid = userGroup.tenantUuid
    , createdAt = userGroup.createdAt
    , updatedAt = now
    }
