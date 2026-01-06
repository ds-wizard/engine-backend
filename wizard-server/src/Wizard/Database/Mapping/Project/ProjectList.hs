module Wizard.Database.Mapping.Project.ProjectList where

import qualified Data.List as L
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Gravatar
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Database.Mapping.Project.ProjectSharing ()
import Wizard.Database.Mapping.Project.ProjectState ()
import Wizard.Database.Mapping.Project.ProjectVisibility ()
import Wizard.Model.Project.ProjectList

instance FromRow ProjectList where
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
    let knowledgeModelPackage = KnowledgeModelPackageSimple {pId = packageId, name = packageName, version = packageVersion}
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
    return $ ProjectList {..}
    where
      parseUserPermission :: U.UUID -> String -> ProjectPermDTO
      parseUserPermission projectUuid permission =
        let parts = splitOn "::" permission
         in ProjectPermDTO
              { projectUuid = projectUuid
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
      parseGroupPermission :: U.UUID -> String -> ProjectPermDTO
      parseGroupPermission projectUuid permission =
        let parts = splitOn "::" permission
         in ProjectPermDTO
              { projectUuid = projectUuid
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
