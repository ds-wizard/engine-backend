module Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM

toCommentThreadsMap :: [QuestionnaireCommentThreadDTO] -> M.Map String [QuestionnaireCommentThreadDTO]
toCommentThreadsMap = foldl go M.empty
  where
    go ::
         M.Map String [QuestionnaireCommentThreadDTO]
      -> QuestionnaireCommentThreadDTO
      -> M.Map String [QuestionnaireCommentThreadDTO]
    go commentThreadsMap thread =
      let threads =
            case M.lookup (thread ^. path) commentThreadsMap of
              Nothing -> []
              Just [] -> []
              Just threads -> threads
       in M.insert (thread ^. path) (thread : threads) commentThreadsMap

toCommentThreadDTO ::
     QuestionnaireCommentThread -> Maybe User -> [QuestionnaireCommentDTO] -> QuestionnaireCommentThreadDTO
toCommentThreadDTO thread mUser comments =
  QuestionnaireCommentThreadDTO
    { _questionnaireCommentThreadDTOUuid = thread ^. uuid
    , _questionnaireCommentThreadDTOPath = thread ^. path
    , _questionnaireCommentThreadDTOResolved = thread ^. resolved
    , _questionnaireCommentThreadDTOComments = comments
    , _questionnaireCommentThreadDTOPrivate = thread ^. private
    , _questionnaireCommentThreadDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , _questionnaireCommentThreadDTOCreatedAt = thread ^. createdAt
    , _questionnaireCommentThreadDTOUpdatedAt = thread ^. updatedAt
    }

toCommentThread :: AddCommentEventChangeDTO -> U.UUID -> Maybe U.UUID -> UTCTime -> QuestionnaireCommentThread
toCommentThread event qtnUuid mCreatedByUuid now =
  QuestionnaireCommentThread
    { _questionnaireCommentThreadUuid = event ^. threadUuid
    , _questionnaireCommentThreadPath = event ^. path
    , _questionnaireCommentThreadResolved = False
    , _questionnaireCommentThreadComments = []
    , _questionnaireCommentThreadPrivate = event ^. private
    , _questionnaireCommentThreadQuestionnaireUuid = qtnUuid
    , _questionnaireCommentThreadCreatedBy = mCreatedByUuid
    , _questionnaireCommentThreadCreatedAt = now
    , _questionnaireCommentThreadUpdatedAt = now
    }

toCommentThread' :: AddCommentEvent -> U.UUID -> QuestionnaireCommentThread
toCommentThread' event qtnUuid =
  QuestionnaireCommentThread
    { _questionnaireCommentThreadUuid = event ^. threadUuid
    , _questionnaireCommentThreadPath = event ^. path
    , _questionnaireCommentThreadResolved = False
    , _questionnaireCommentThreadComments = []
    , _questionnaireCommentThreadPrivate = event ^. private
    , _questionnaireCommentThreadQuestionnaireUuid = qtnUuid
    , _questionnaireCommentThreadCreatedBy = event ^. createdBy
    , _questionnaireCommentThreadCreatedAt = event ^. createdAt
    , _questionnaireCommentThreadUpdatedAt = event ^. createdAt
    }

toCommentDTO :: QuestionnaireComment -> Maybe User -> QuestionnaireCommentDTO
toCommentDTO comment mUser =
  QuestionnaireCommentDTO
    { _questionnaireCommentDTOUuid = comment ^. uuid
    , _questionnaireCommentDTOText = comment ^. text
    , _questionnaireCommentDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , _questionnaireCommentDTOCreatedAt = comment ^. createdAt
    , _questionnaireCommentDTOUpdatedAt = comment ^. updatedAt
    }

toComment :: AddCommentEventChangeDTO -> Maybe U.UUID -> UTCTime -> QuestionnaireComment
toComment event mCreatedByUuid now =
  QuestionnaireComment
    { _questionnaireCommentUuid = event ^. commentUuid
    , _questionnaireCommentText = event ^. text
    , _questionnaireCommentThreadUuid = event ^. threadUuid
    , _questionnaireCommentCreatedBy = mCreatedByUuid
    , _questionnaireCommentCreatedAt = now
    , _questionnaireCommentUpdatedAt = now
    }

toComment' :: AddCommentEvent -> QuestionnaireComment
toComment' event =
  QuestionnaireComment
    { _questionnaireCommentUuid = event ^. commentUuid
    , _questionnaireCommentText = event ^. text
    , _questionnaireCommentThreadUuid = event ^. threadUuid
    , _questionnaireCommentCreatedBy = event ^. createdBy
    , _questionnaireCommentCreatedAt = event ^. createdAt
    , _questionnaireCommentUpdatedAt = event ^. createdAt
    }
