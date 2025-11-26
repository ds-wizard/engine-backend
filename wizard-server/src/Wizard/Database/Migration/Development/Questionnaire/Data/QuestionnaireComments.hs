module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments where

import Control.Monad.Reader (liftIO)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned
import Wizard.Model.Questionnaire.QuestionnaireUtil
import Wizard.Model.User.User
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.User.UserMapper

qtnThreadsDto :: M.Map String [QuestionnaireCommentThreadList]
qtnThreadsDto = M.fromList [(cmtQ1_path, [cmtQ1_t1Dto]), (cmtQ2_path, [cmtQ2_t1Dto])]

cmtQ1_path :: String
cmtQ1_path = createReplyKey [chapter1.uuid, question1.uuid]

cmtQ1_t1 :: QuestionnaireCommentThread
cmtQ1_t1 =
  QuestionnaireCommentThread
    { uuid = u' "f1de85a9-7f22-4d0c-bc23-3315cc4c85d7"
    , path = cmtQ1_path
    , resolved = False
    , comments = [cmtQ1_t1_1, cmtQ1_t1_2]
    , private = False
    , questionnaireUuid = u' "af984a75-56e3-49f8-b16f-d6b99599910a"
    , tenantUuid = defaultTenantUuid
    , assignedTo = Nothing
    , assignedBy = Nothing
    , notificationRequired = False
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

create_cmtQ1_t1 :: U.UUID -> IO QuestionnaireCommentThread
create_cmtQ1_t1 qtnUuid = do
  threadUuid <- liftIO generateUuid
  return $
    QuestionnaireCommentThread
      { uuid = threadUuid
      , path = cmtQ1_path
      , resolved = False
      , comments = []
      , private = False
      , questionnaireUuid = qtnUuid
      , tenantUuid = defaultTenantUuid
      , assignedTo = Nothing
      , assignedBy = Nothing
      , notificationRequired = False
      , createdBy = Just userAlbert.uuid
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }

cmtQ1_t1Dto :: QuestionnaireCommentThreadList
cmtQ1_t1Dto = toCommentThreadList cmtQ1_t1 Nothing (Just userAlbert) [cmtQ1_t1_1Dto, cmtQ1_t1_2Dto]

cmtQ1_t1WithEditedCmt :: QuestionnaireCommentThread
cmtQ1_t1WithEditedCmt = cmtQ1_t1 {comments = [cmtQ1_t1_1Edited, cmtQ1_t1_2]}

cmtQ1_t1WithDeletedCmt :: QuestionnaireCommentThread
cmtQ1_t1WithDeletedCmt = cmtQ1_t1 {comments = [cmtQ1_t1_2]}

cmtQ1_t1Resolved :: QuestionnaireCommentThread
cmtQ1_t1Resolved = cmtQ1_t1 {resolved = True}

cmtQ1_t1_1 :: QuestionnaireComment
cmtQ1_t1_1 =
  QuestionnaireComment
    { uuid = u' "a2d4a1f6-6148-43a0-98d1-158176863a3c"
    , text = "1st comment to 1st question"
    , threadUuid = cmtQ1_t1.uuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

create_cmtQ1_t1_1 :: U.UUID -> IO QuestionnaireComment
create_cmtQ1_t1_1 threadUuid = do
  commentUuid <- liftIO generateUuid
  return $
    QuestionnaireComment
      { uuid = commentUuid
      , text = "1st comment to 1st question"
      , threadUuid = threadUuid
      , tenantUuid = defaultTenantUuid
      , createdBy = Just userAlbert.uuid
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }

cmtQ1_t1_1Dto :: QuestionnaireCommentList
cmtQ1_t1_1Dto = toCommentList cmtQ1_t1_1 (Just userAlbert)

cmtQ1_t1_1Edited :: QuestionnaireComment
cmtQ1_t1_1Edited = cmtQ1_t1_1 {text = "EDITED: 1st comment to 1st question"}

cmtQ1_t1_2 :: QuestionnaireComment
cmtQ1_t1_2 =
  QuestionnaireComment
    { uuid = u' "a9861528-7ca9-48d8-917c-3bf9f240bdf3"
    , text = "2nd comment to 1st question"
    , threadUuid = cmtQ1_t1.uuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

create_cmtQ1_t1_2 :: U.UUID -> IO QuestionnaireComment
create_cmtQ1_t1_2 threadUuid = do
  commentUuid <- liftIO generateUuid
  return $
    QuestionnaireComment
      { uuid = commentUuid
      , text = "2nd comment to 1st question"
      , threadUuid = threadUuid
      , tenantUuid = defaultTenantUuid
      , createdBy = Just userAlbert.uuid
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }

cmtQ1_t1_2Dto :: QuestionnaireCommentList
cmtQ1_t1_2Dto = toCommentList cmtQ1_t1_2 (Just userAlbert)

cmtQ2_path :: String
cmtQ2_path = createReplyKey [chapter1.uuid, question2.uuid]

cmtQ2_t1 :: QuestionnaireCommentThread
cmtQ2_t1 =
  QuestionnaireCommentThread
    { uuid = u' "2b8681dc-54b5-4bc4-bf9f-a1ec6ad37823"
    , path = cmtQ2_path
    , resolved = False
    , comments = [cmtQ2_t1_1]
    , private = False
    , questionnaireUuid = u' "af984a75-56e3-49f8-b16f-d6b99599910a"
    , tenantUuid = defaultTenantUuid
    , assignedTo = Nothing
    , assignedBy = Nothing
    , notificationRequired = False
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

cmtQ2_t1Dto :: QuestionnaireCommentThreadList
cmtQ2_t1Dto = toCommentThreadList cmtQ2_t1 Nothing (Just userAlbert) [cmtQ2_t1_1Dto]

cmtQ2_t1_1 :: QuestionnaireComment
cmtQ2_t1_1 =
  QuestionnaireComment
    { uuid = u' "e9827a92-ecfd-4410-8809-ea761fe03bd3"
    , text = "1nd comment to 2st question"
    , threadUuid = cmtQ2_t1.uuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

cmtQ2_t1_1Dto :: QuestionnaireCommentList
cmtQ2_t1_1Dto = toCommentList cmtQ2_t1_1 (Just userAlbert)

cmtAssigned :: QuestionnaireCommentThreadAssigned
cmtAssigned =
  QuestionnaireCommentThreadAssigned
    { questionnaireUuid = u' "af984a75-56e3-49f8-b16f-d6b99599910a"
    , questionnaireName = "My Private Questionnaire"
    , commentThreadUuid = cmtQ1_t1.uuid
    , path = cmtQ1_t1.path
    , resolved = cmtQ1_t1.resolved
    , private = cmtQ1_t1.private
    , text = cmtQ1_t1_1.text
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , updatedAt = dt' 2018 1 21
    }
