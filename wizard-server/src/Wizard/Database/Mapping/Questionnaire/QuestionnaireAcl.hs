module Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl where

import qualified Data.List as L
import qualified Data.UUID as U
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Gravatar
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()

loadPermissions :: U.UUID -> RowParser [QuestionnairePermDTO]
loadPermissions qtnUuid = do
  mUserPermissions <- fieldWith (optionalField fromField)
  let userPermissions =
        case mUserPermissions of
          Just userPermissions -> L.sort . fmap (parseUserPermission qtnUuid) . fromPGArray $ userPermissions
          Nothing -> []
  mGroupPermissions <- fieldWith (optionalField fromField)
  let groupPermissions =
        case mGroupPermissions of
          Just groupPermissions -> L.sort . fmap (parseGroupPermission qtnUuid) . fromPGArray $ groupPermissions
          Nothing -> []
  return $ userPermissions ++ groupPermissions

parseUserPermission :: U.UUID -> String -> QuestionnairePermDTO
parseUserPermission qtnUuid permission =
  let parts = splitOn "::" permission
   in QuestionnairePermDTO
        { questionnaireUuid = qtnUuid
        , perms = splitOn "," . replace "}" "" . replace "{" "" $ parts !! 1
        , member =
            UserMemberDTO
              { uuid = u' (parts !! 2)
              , firstName = parts !! 3
              , lastName = parts !! 4
              , gravatarHash = createGravatarHash $ parts !! 5
              , imageUrl =
                  case parts !! 6 of
                    "" -> Nothing
                    imageUrl -> Just imageUrl
              }
        }
parseGroupPermission :: U.UUID -> String -> QuestionnairePermDTO
parseGroupPermission qtnUuid permission =
  let parts = splitOn "::" permission
   in QuestionnairePermDTO
        { questionnaireUuid = qtnUuid
        , perms = splitOn "," . replace "}" "" . replace "{" "" $ parts !! 1
        , member =
            UserGroupMemberDTO
              { uuid = u' (parts !! 2)
              , name = parts !! 3
              , description = if length parts == 6 then Just $ parts !! 5 else Nothing
              , private =
                  case parts !! 4 of
                    "t" -> True
                    _ -> False
              }
        }
