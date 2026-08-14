module WizardLib.Public.Database.Mapping.User.UserSimple where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import WizardLib.Public.Model.User.UserSimple

instance FromRow UserSimple where
  fromRow = do
    uuid <- field
    firstName <- field
    lastName <- field
    email <- field
    imageUrl <- field
    affiliation <- field
    return $ UserSimple {..}

fieldUserSimple :: RowParser UserSimple
fieldUserSimple = do
  uuid <- field
  firstName <- field
  lastName <- field
  email <- field
  imageUrl <- field
  affiliation <- field
  return UserSimple {..}

fieldUserSimple' :: RowParser (Maybe UserSimple)
fieldUserSimple' = do
  mCreatedByUuid <- field
  mCreatedByFirstName <- field
  mCreatedByLastName <- field
  mCreatedByEmail <- field
  mCreatedByImageUrl <- field
  mCreatedByAffiliation <- field
  case (mCreatedByUuid, mCreatedByFirstName, mCreatedByLastName, mCreatedByEmail, mCreatedByImageUrl, mCreatedByAffiliation) of
    (Just uuid, Just firstName, Just lastName, Just email, imageUrl, affiliation) -> return $ Just UserSimple {..}
    _ -> return Nothing
