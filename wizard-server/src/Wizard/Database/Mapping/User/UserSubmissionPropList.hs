module Wizard.Database.Mapping.User.UserSubmissionPropList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.User.UserSubmissionPropList

instance FromRow UserSubmissionPropList where
  fromRow = do
    sId <- field
    name <- field
    values <- fieldWith fromJSONField
    return $ UserSubmissionPropList {..}
