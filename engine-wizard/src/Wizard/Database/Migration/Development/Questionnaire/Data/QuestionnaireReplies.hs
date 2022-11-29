module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Questionnaire.QuestionnaireUtil
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireReply
import qualified Wizard.Service.User.UserMapper as UM

fReplies :: M.Map String Reply
fReplies =
  M.fromList
    [ rQ1
    , rQ2
    , rQ2_aYes_fuQ1
    , rQ3
    , rQ4
    , rQ4_it1_q5
    , rQ4_it1_q5_it1_question7
    , rQ4_it1_q5_it1_question8
    , rQ4_it1_q6
    , rQ4_it2_q5
    , rQ4_it2_q6
    , rQ9
    , rQ10
    , rQ11
    ]

fRepliesWithUpdated :: M.Map String Reply
fRepliesWithUpdated =
  M.fromList
    [ rQ1Updated
    , rQ2
    , rQ2_aYes_fuQ1
    , rQ3
    , rQ4
    , rQ4_it1_q5
    , rQ4_it1_q5_it1_question7
    , rQ4_it1_q5_it1_question8
    , rQ4_it1_q6
    , rQ4_it2_q5
    , rQ4_it2_q6
    , rQ9
    , rQ10
    , rQ11
    ]

fRepliesWithDeleted :: M.Map String Reply
fRepliesWithDeleted =
  M.fromList
    [ rQ2
    , rQ2_aYes_fuQ1
    , rQ3
    , rQ4
    , rQ4_it1_q5
    , rQ4_it1_q5_it1_question7
    , rQ4_it1_q5_it1_question8
    , rQ4_it1_q6
    , rQ4_it2_q5
    , rQ4_it2_q6
    , rQ9
    , rQ10
    , rQ11
    ]

fRepliesEdited :: M.Map String Reply
fRepliesEdited = M.fromList [rQ1, rQ2, rQ2_aYes_fuQ1, rQ3, rQ9, rQ10, rQ11]

rQ1 :: ReplyTuple
rQ1 =
  ( createReplyKey [chapter1.uuid, question1.uuid]
  , Reply
      { value = StringReply {sValue = "Reply to 1st question"}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ1Updated :: ReplyTuple
rQ1Updated =
  ( createReplyKey [chapter1.uuid, question1.uuid]
  , Reply
      { value = StringReply {sValue = "Updated Reply to 1st question"}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ2 :: ReplyTuple
rQ2 =
  ( createReplyKey [chapter1.uuid, question2.uuid]
  , Reply
      { value = AnswerReply {aValue = q2_answerYes.uuid}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ2_aYes_fuQ1 :: ReplyTuple
rQ2_aYes_fuQ1 =
  ( createReplyKey [chapter1.uuid, question2.uuid, q2_answerYes.uuid, q2_aYes_fuQuestion1.uuid]
  , Reply
      { value = AnswerReply {aValue = q2_aYes_fuq1_answerNo.uuid}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

unused_rQ2_aYes_fuQ1_aYes_fuq2 :: ReplyTuple
unused_rQ2_aYes_fuQ1_aYes_fuq2 =
  ( createReplyKey
      [ chapter1.uuid
      , question2.uuid
      , q2_answerYes.uuid
      , q2_aYes_fuQuestion1.uuid
      , q2_aYes_fuq1_answerYes.uuid
      , q2_aYes_fuq1_aYes_fuQuestion2.uuid
      ]
  , Reply
      { value = AnswerReply {aValue = q2_aYes_fuq1_aYes_fuq2_answerNo.uuid}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ3 :: ReplyTuple
rQ3 =
  ( createReplyKey [chapter2.uuid, question3.uuid]
  , Reply
      { value = AnswerReply {aValue = q3_answerNo.uuid}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

-- ------------------------------------------------------------
rQ4 :: ReplyTuple
rQ4 =
  ( createReplyKey [chapter2.uuid, question4.uuid]
  , Reply
      { value = ItemListReply {ilValue = [rQ4_it1, rQ4_it2]}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ4_it1 :: U.UUID
rQ4_it1 = u' "97e42df3-f0f6-40f8-83ab-375a1340e8ab"

rQ4_it1_q5 :: ReplyTuple
rQ4_it1_q5 =
  ( createReplyKey [chapter2.uuid, question4.uuid, rQ4_it1, q4_it1_question5.uuid]
  , Reply
      { value = ItemListReply {ilValue = [rQ4_it1_q5_it1]}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ4_it1_q5_it1 :: U.UUID
rQ4_it1_q5_it1 = u' "12c243e0-f300-4178-ae4e-0b30d01c6f73"

rQ4_it1_q5_it1_question7 :: ReplyTuple
rQ4_it1_q5_it1_question7 =
  ( createReplyKey
      [ chapter2.uuid
      , question4.uuid
      , rQ4_it1
      , q4_it1_question5.uuid
      , rQ4_it1_q5_it1
      , q4_it1_q5_it2_question7.uuid
      ]
  , Reply
      { value = StringReply {sValue = "Ai1: q5: Ai1: Reply to 7th question"}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ4_it1_q5_it1_question8 :: ReplyTuple
rQ4_it1_q5_it1_question8 =
  ( createReplyKey
      [ chapter2.uuid
      , question4.uuid
      , rQ4_it1
      , q4_it1_question5.uuid
      , rQ4_it1_q5_it1
      , q4_it1_q5_it2_question8.uuid
      ]
  , Reply
      { value = StringReply {sValue = "Ai1: q5: Ai1: Reply to 8th question"}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ4_it1_q6 :: ReplyTuple
rQ4_it1_q6 =
  ( createReplyKey [chapter2.uuid, question4.uuid, rQ4_it1, q4_it1_question6.uuid]
  , Reply
      { value = AnswerReply {aValue = q4_it1_q6_answerNo.uuid}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

-- ------------------------------------------------------------
rQ4_it2 :: U.UUID
rQ4_it2 = u' "aed4bcbc-0c63-4bf6-b954-4561e92babfa"

rQ4_it2_q5 :: ReplyTuple
rQ4_it2_q5 =
  ( createReplyKey [chapter2.uuid, question4.uuid, rQ4_it2, q4_it1_question5.uuid]
  , Reply
      { value = ItemListReply {ilValue = []}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ4_it2_q6 :: ReplyTuple
rQ4_it2_q6 =
  ( createReplyKey [chapter2.uuid, question4.uuid, rQ4_it2, q4_it1_question6.uuid]
  , Reply
      { value = AnswerReply {aValue = q4_it1_q6_answerNo.uuid}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

-- ------------------------------------------------------------
rQ9 :: ReplyTuple
rQ9 =
  ( createReplyKey [chapter3.uuid, question9.uuid]
  , Reply
      { value = IntegrationReply {iValue = PlainType rQ9Value}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ9Value :: String
rQ9Value = "Plain reply to 9st question"

rQ9WithNewType :: ReplyTuple
rQ9WithNewType =
  ( createReplyKey [chapter3.uuid, question9.uuid]
  , Reply
      { value = StringReply {sValue = rQ9Value}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ10 :: ReplyTuple
rQ10 =
  ( createReplyKey [chapter3.uuid, question10.uuid]
  , Reply
      { value = IntegrationReply {iValue = rQ10IntValue}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )

rQ10IntValue :: IntegrationReplyType
rQ10IntValue =
  IntegrationType {intId = "bsg-p000007", value = "Integration reply to 9st question"}

-- ------------------------------------------------------------------------
rQ11 :: ReplyTuple
rQ11 =
  ( createReplyKey [chapter3.uuid, question11.uuid]
  , Reply
      { value = MultiChoiceReply {mcValue = [q11_choice2.uuid]}
      , createdBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
      }
  )
