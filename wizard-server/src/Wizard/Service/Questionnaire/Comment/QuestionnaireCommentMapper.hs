module Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM

toCommentThreadsMap :: [QuestionnaireCommentThreadList] -> M.Map String [QuestionnaireCommentThreadList]
toCommentThreadsMap = foldl go M.empty
  where
    go
      :: M.Map String [QuestionnaireCommentThreadList]
      -> QuestionnaireCommentThreadList
      -> M.Map String [QuestionnaireCommentThreadList]
    go commentThreadsMap thread =
      let threads =
            case M.lookup thread.path commentThreadsMap of
              Nothing -> []
              Just [] -> []
              Just threads -> threads
       in M.insert thread.path (thread : threads) commentThreadsMap

toCommentThreadList :: QuestionnaireCommentThread -> Maybe User -> Maybe User -> [QuestionnaireCommentList] -> QuestionnaireCommentThreadList
toCommentThreadList thread mAssignedTo mCreatedBy comments =
  QuestionnaireCommentThreadList
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

toCommentThread :: AddCommentEventChangeDTO -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> QuestionnaireCommentThread
toCommentThread event qtnUuid tenantUuid mCreatedByUuid now =
  QuestionnaireCommentThread
    { uuid = event.threadUuid
    , path = event.path
    , resolved = False
    , comments = []
    , private = event.private
    , questionnaireUuid = qtnUuid
    , tenantUuid = tenantUuid
    , assignedTo = Nothing
    , assignedBy = Nothing
    , notificationRequired = False
    , createdBy = mCreatedByUuid
    , createdAt = now
    , updatedAt = now
    }

toCommentList :: QuestionnaireComment -> Maybe User -> QuestionnaireCommentList
toCommentList comment mUser =
  QuestionnaireCommentList
    { uuid = comment.uuid
    , text = comment.text
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) mUser
    , createdAt = comment.createdAt
    , updatedAt = comment.updatedAt
    }

toComment :: AddCommentEventChangeDTO -> U.UUID -> Maybe U.UUID -> UTCTime -> QuestionnaireComment
toComment event tenantUuid mCreatedByUuid now =
  QuestionnaireComment
    { uuid = event.commentUuid
    , text = event.text
    , threadUuid = event.threadUuid
    , tenantUuid = tenantUuid
    , createdBy = mCreatedByUuid
    , createdAt = now
    , updatedAt = now
    }
