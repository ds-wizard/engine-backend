module Wizard.Database.Mapping.Questionnaire.QuestionnaireList where

import qualified Data.List as L
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Gravatar
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireState ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnaireList
import WizardLib.KnowledgeModel.Model.Package.PackageSimple

instance FromRow QuestionnaireList where
  fromRow = do
    uuid <- field
    name <- field
    description <- field
    visibility <- field
    sharing <- field
    isTemplate <- field
    createdAt <- field
    updatedAt <- field
    state <- field
    packageId <- field
    packageName <- field
    packageVersion <- field
    let package = PackageSimple {pId = packageId, name = packageName, version = packageVersion}
    mUserPermissions <- fieldWith (optionalField fromField)
    let userPermissions =
          case mUserPermissions of
            Just userPermissions -> L.sort . fmap (parseUserPermission uuid) . fromPGArray $ userPermissions
            Nothing -> []
    mGroupPermissions <- fieldWith (optionalField fromField)
    let groupPermissions =
          case mGroupPermissions of
            Just groupPermissions -> L.sort . fmap (parseGroupPermission uuid) . fromPGArray $ groupPermissions
            Nothing -> []
    let permissions = userPermissions ++ groupPermissions
    return $ QuestionnaireList {..}
    where
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
