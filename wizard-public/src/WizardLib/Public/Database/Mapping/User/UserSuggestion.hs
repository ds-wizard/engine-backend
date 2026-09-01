module WizardLib.Public.Database.Mapping.User.UserSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import WizardLib.Public.Model.User.UserSuggestion

instance FromRow UserSuggestion where
  fromRow = do
    uuid <- field
    firstName <- field
    lastName <- field
    gravatarHash <- field
    imageUrl <- field
    affiliation <- field
    return $ UserSuggestion {..}

fieldUserSuggestion :: RowParser UserSuggestion
fieldUserSuggestion = do
  uuid <- field
  firstName <- field
  lastName <- field
  gravatarHash <- field
  imageUrl <- field
  affiliation <- field
  return UserSuggestion {..}

fieldUserSuggestion' :: RowParser (Maybe UserSuggestion)
fieldUserSuggestion' = do
  mCreatedByUuid <- field
  mCreatedByFirstName <- field
  mCreatedByLastName <- field
  mCreatedByGravatarHash <- field
  mCreatedByImageUrl <- field
  mCreatedByAffiliation <- field
  case (mCreatedByUuid, mCreatedByFirstName, mCreatedByLastName, mCreatedByGravatarHash, mCreatedByImageUrl, mCreatedByAffiliation) of
    (Just uuid, Just firstName, Just lastName, Just gravatarHash, imageUrl, affiliation) -> return $ Just UserSuggestion {..}
    _ -> return Nothing
