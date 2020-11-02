module Wizard.Database.BSON.User.User where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Acl.Acl ()
import Wizard.Model.User.User

instance ToBSON User

instance FromBSON User

instance ToBSON UserSubmissionProps

instance FromBSON UserSubmissionProps
