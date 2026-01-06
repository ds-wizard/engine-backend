module Wizard.Service.Project.Comment.ProjectCommentMapper where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Model.Project.Comment.ProjectComment
import Wizard.Model.Project.Comment.ProjectCommentList
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM

toCommentThreadsMap :: [ProjectCommentThreadList] -> M.Map String [ProjectCommentThreadList]
toCommentThreadsMap = foldl go M.empty
  where
    go
      :: M.Map String [ProjectCommentThreadList]
      -> ProjectCommentThreadList
      -> M.Map String [ProjectCommentThreadList]
    go commentThreadsMap thread =
      let threads =
            case M.lookup thread.path commentThreadsMap of
              Nothing -> []
              Just [] -> []
              Just threads -> threads
       in M.insert thread.path (thread : threads) commentThreadsMap

toCommentThreadList :: ProjectCommentThread -> Maybe User -> Maybe User -> [ProjectCommentList] -> ProjectCommentThreadList
toCommentThreadList thread mAssignedTo mCreatedBy comments =
  ProjectCommentThreadList
    { uuid = thread.uuid
    , path = thread.path
    , resolved = thread.resolved
    , comments = comments
    , private = thread.private
    , assignedTo = fmap (UM.toSuggestion . UM.toSimple) mAssignedTo
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) mCreatedBy
    , createdAt = thread.createdAt
    , updatedAt = thread.updatedAt
    }

toCommentThread :: AddCommentEventChangeDTO -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> ProjectCommentThread
toCommentThread event projectUuid tenantUuid mCreatedByUuid now =
  ProjectCommentThread
    { uuid = event.threadUuid
    , path = event.path
    , resolved = False
    , comments = []
    , private = event.private
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , assignedTo = Nothing
    , assignedBy = Nothing
    , notificationRequired = False
    , createdBy = mCreatedByUuid
    , createdAt = now
    , updatedAt = now
    }

toCommentList :: ProjectComment -> Maybe User -> ProjectCommentList
toCommentList comment mUser =
  ProjectCommentList
    { uuid = comment.uuid
    , text = comment.text
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) mUser
    , createdAt = comment.createdAt
    , updatedAt = comment.updatedAt
    }

toComment :: AddCommentEventChangeDTO -> U.UUID -> Maybe U.UUID -> UTCTime -> ProjectComment
toComment event tenantUuid mCreatedByUuid now =
  ProjectComment
    { uuid = event.commentUuid
    , text = event.text
    , threadUuid = event.threadUuid
    , tenantUuid = tenantUuid
    , createdBy = mCreatedByUuid
    , createdAt = now
    , updatedAt = now
    }
