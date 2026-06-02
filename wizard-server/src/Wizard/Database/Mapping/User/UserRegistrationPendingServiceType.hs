module Wizard.Database.Mapping.User.UserRegistrationPendingServiceType where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.User.UserRegistrationPendingServiceType

instance ToField UserRegistrationPendingServiceType where
  toField = toFieldGenericEnum

instance FromField UserRegistrationPendingServiceType where
  fromField = fromFieldGenericEnum
