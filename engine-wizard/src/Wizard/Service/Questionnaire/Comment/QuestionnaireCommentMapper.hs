module Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM

toCommentThreadsMap :: [QuestionnaireCommentThreadDTO] -> M.Map String [QuestionnaireCommentThreadDTO]
toCommentThreadsMap = foldl go M.empty
  where
    go
      :: M.Map String [QuestionnaireCommentThreadDTO]
      -> QuestionnaireCommentThreadDTO
      -> M.Map String [QuestionnaireCommentThreadDTO]
    go commentThreadsMap thread =
      let threads =
            case M.lookup thread.path commentThreadsMap of
              Nothing -> []
              Just [] -> []
              Just threads -> threads
       in M.insert thread.path (thread : threads) commentThreadsMap

toCommentThreadDTO
  :: QuestionnaireCommentThread -> Maybe User -> [QuestionnaireCommentDTO] -> QuestionnaireCommentThreadDTO
toCommentThreadDTO thread mUser comments =
  QuestionnaireCommentThreadDTO
    { uuid = thread.uuid
    , path = thread.path
    , resolved = thread.resolved
    , comments = comments
    , private = thread.private
    , createdBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , createdAt = thread.createdAt
    , updatedAt = thread.updatedAt
    }

toCommentThread :: AddCommentEventChangeDTO -> U.UUID -> Maybe U.UUID -> UTCTime -> QuestionnaireCommentThread
toCommentThread event qtnUuid mCreatedByUuid now =
  QuestionnaireCommentThread
    { uuid = event.threadUuid
    , path = event.path
    , resolved = False
    , comments = []
    , private = event.private
    , questionnaireUuid = qtnUuid
    , createdBy = mCreatedByUuid
    , createdAt = now
    , updatedAt = now
    }

toCommentDTO :: QuestionnaireComment -> Maybe User -> QuestionnaireCommentDTO
toCommentDTO comment mUser =
  QuestionnaireCommentDTO
    { uuid = comment.uuid
    , text = comment.text
    , createdBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , createdAt = comment.createdAt
    , updatedAt = comment.updatedAt
    }

toComment :: AddCommentEventChangeDTO -> Maybe U.UUID -> UTCTime -> QuestionnaireComment
toComment event mCreatedByUuid now =
  QuestionnaireComment
    { uuid = event.commentUuid
    , text = event.text
    , threadUuid = event.threadUuid
    , createdBy = mCreatedByUuid
    , createdAt = now
    , updatedAt = now
    }
