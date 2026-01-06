module Wizard.Database.Mapping.Project.Comment.ProjectCommentThread where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Date
import Shared.Common.Util.String (splitOn)
import Shared.Common.Util.Uuid
import Wizard.Model.Project.Comment.ProjectComment

instance ToRow ProjectCommentThread where
  toRow ProjectCommentThread {..} =
    [ toField uuid
    , toField path
    , toField resolved
    , toField private
    , toField projectUuid
    , toField createdBy
    , toField createdAt
    , toField updatedAt
    , toField tenantUuid
    , toField assignedTo
    , toField assignedBy
    , toField notificationRequired
    ]

instance FromRow ProjectCommentThread where
  fromRow = do
    uuid <- field
    path <- field
    resolved <- field
    private <- field
    projectUuid <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    tenantUuid <- field
    assignedTo <- field
    assignedBy <- field
    notificationRequired <- field
    commentsArray <- fromPGArray <$> field
    let comments = fmap parseComment commentsArray
    return $ ProjectCommentThread {..}

parseComment :: String -> ProjectComment
parseComment commentS =
  let parts = splitOn ":::::" commentS
   in ProjectComment
        { uuid = u' (head parts)
        , text = parts !! 1
        , threadUuid = u' (parts !! 2)
        , tenantUuid = u' (parts !! 3)
        , createdBy =
            case parts !! 4 of
              "" -> Nothing
              u -> Just . u' $ u
        , createdAt = parsePostgresDateTime' $ parts !! 5
        , updatedAt = parsePostgresDateTime' $ parts !! 6
        }
