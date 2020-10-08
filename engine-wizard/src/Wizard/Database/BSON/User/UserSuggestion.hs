module Wizard.Database.BSON.User.UserSuggestion where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.User.UserSuggestion

instance ToBSON UserSuggestion

instance FromBSON UserSuggestion
