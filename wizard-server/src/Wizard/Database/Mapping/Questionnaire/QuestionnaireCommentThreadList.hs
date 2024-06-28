module Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThreadList where

import qualified Data.List as L
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Date
import Shared.Common.Util.Gravatar
import Shared.Common.Util.String (splitOn)
import Shared.Common.Util.Uuid
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

instance FromRow QuestionnaireCommentThreadList where
  fromRow = do
    uuid <- field
    path <- field
    resolved <- field
    private <- field
    createdAt <- field
    updatedAt <- field
    mAssignedToUuid <- fieldWith (optionalField fromField)
    mAssignedToFirstName <- fieldWith (optionalField fromField)
    mAssignedToLastName <- fieldWith (optionalField fromField)
    mAssignedToEmail <- fieldWith (optionalField fromField)
    mAssignedToImageUrl <- fieldWith (optionalField fromField)
    let assignedTo =
          case (mAssignedToUuid, mAssignedToFirstName, mAssignedToLastName, mAssignedToEmail) of
            (Just assignedToUuid, Just assignedToFirstName, Just assignedToLastName, Just assignedToEmail) ->
              Just $
                UserSuggestionDTO
                  { uuid = assignedToUuid
                  , firstName = assignedToFirstName
                  , lastName = assignedToLastName
                  , imageUrl = mAssignedToImageUrl
                  , gravatarHash = createGravatarHash assignedToEmail
                  }
            _ -> Nothing
    mCreatedByUuid <- fieldWith (optionalField fromField)
    mCreatedByFirstName <- fieldWith (optionalField fromField)
    mCreatedByLastName <- fieldWith (optionalField fromField)
    mCreatedByEmail <- fieldWith (optionalField fromField)
    mCreatedByImageUrl <- fieldWith (optionalField fromField)
    let createdBy =
          case (mCreatedByUuid, mCreatedByFirstName, mCreatedByLastName, mCreatedByEmail) of
            (Just createdByUuid, Just createdByFirstName, Just createdByLastName, Just createdByEmail) ->
              Just $
                UserSuggestionDTO
                  { uuid = createdByUuid
                  , firstName = createdByFirstName
                  , lastName = createdByLastName
                  , imageUrl = mCreatedByImageUrl
                  , gravatarHash = createGravatarHash createdByEmail
                  }
            _ -> Nothing
    commentsArray <- fromPGArray <$> field
    let comments = L.sort . fmap parseComment $ commentsArray
    return QuestionnaireCommentThreadList {..}

parseComment :: String -> QuestionnaireCommentList
parseComment commentS =
  let parts = splitOn "<:::::>" commentS
   in QuestionnaireCommentList
        { uuid = u' (head parts)
        , text = parts !! 1
        , createdAt = parsePostgresDateTime' $ parts !! 2
        , updatedAt = parsePostgresDateTime' $ parts !! 3
        , createdBy =
            case parts !! 4 of
              "" -> Nothing
              _ ->
                Just $
                  UserSuggestionDTO
                    { uuid = u' $ parts !! 4
                    , firstName = parts !! 5
                    , lastName = parts !! 6
                    , imageUrl =
                        case parts !! 8 of
                          "" -> Nothing
                          value -> Just value
                    , gravatarHash = createGravatarHash $ parts !! 7
                    }
        }
