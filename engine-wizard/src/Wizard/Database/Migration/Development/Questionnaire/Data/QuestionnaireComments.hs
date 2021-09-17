module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Model.Questionnaire.QuestionnaireUtil
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireComment
import qualified Wizard.Service.User.UserMapper as UM

qtnThreads :: M.Map String [QuestionnaireCommentThread]
qtnThreads = M.fromList [(cmtQ1_path, [cmtQ1_t1])]

cmtQ1_path :: String
cmtQ1_path = createReplyKey [chapter1 ^. uuid, question1 ^. uuid]

cmtQ1_t1 :: QuestionnaireCommentThread
cmtQ1_t1 =
  QuestionnaireCommentThread
    { _questionnaireCommentThreadUuid = u' "f1de85a9-7f22-4d0c-bc23-3315cc4c85d7"
    , _questionnaireCommentThreadResolved = False
    , _questionnaireCommentThreadComments = [cmtQ1_t1_1, cmtQ1_t1_2]
    , _questionnaireCommentThreadCreatedBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
    , _questionnaireCommentThreadCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireCommentThreadUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

cmtQ1_t1WithEditedCmt :: QuestionnaireCommentThread
cmtQ1_t1WithEditedCmt = cmtQ1_t1 {_questionnaireCommentThreadComments = [cmtQ1_t1_1Edited, cmtQ1_t1_2]}

cmtQ1_t1WithDeletedCmt :: QuestionnaireCommentThread
cmtQ1_t1WithDeletedCmt = cmtQ1_t1 {_questionnaireCommentThreadComments = [cmtQ1_t1_2]}

cmtQ1_t1Resolved :: QuestionnaireCommentThread
cmtQ1_t1Resolved = cmtQ1_t1 {_questionnaireCommentThreadResolved = True}

cmtQ1_t1_1 :: QuestionnaireComment
cmtQ1_t1_1 =
  QuestionnaireComment
    { _questionnaireCommentUuid = u' "a2d4a1f6-6148-43a0-98d1-158176863a3c"
    , _questionnaireCommentText = "1st comment to 1st question"
    , _questionnaireCommentCreatedBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
    , _questionnaireCommentCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireCommentUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

cmtQ1_t1_1Edited :: QuestionnaireComment
cmtQ1_t1_1Edited = cmtQ1_t1_1 {_questionnaireCommentText = "EDITED: 1st comment to 1st question"}

cmtQ1_t1_2 :: QuestionnaireComment
cmtQ1_t1_2 =
  QuestionnaireComment
    { _questionnaireCommentUuid = u' "09861528-7ca9-48d8-917c-3bf9f240bdf3"
    , _questionnaireCommentText = "2nd comment to 1st question"
    , _questionnaireCommentCreatedBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
    , _questionnaireCommentCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireCommentUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

cmtQ2_path :: String
cmtQ2_path = createReplyKey [chapter1 ^. uuid, question2 ^. uuid]

cmtQ2_t1 :: QuestionnaireCommentThread
cmtQ2_t1 =
  QuestionnaireCommentThread
    { _questionnaireCommentThreadUuid = u' "2b8681dc-54b5-4bc4-bf9f-a1ec6ad37823"
    , _questionnaireCommentThreadResolved = False
    , _questionnaireCommentThreadComments = [cmtQ2_t1_1]
    , _questionnaireCommentThreadCreatedBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
    , _questionnaireCommentThreadCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireCommentThreadUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

cmtQ2_t1_1 :: QuestionnaireComment
cmtQ2_t1_1 =
  QuestionnaireComment
    { _questionnaireCommentUuid = u' "e9827a92-ecfd-4410-8809-ea761fe03bd3"
    , _questionnaireCommentText = "1nd comment to 2st question"
    , _questionnaireCommentCreatedBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
    , _questionnaireCommentCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireCommentUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
