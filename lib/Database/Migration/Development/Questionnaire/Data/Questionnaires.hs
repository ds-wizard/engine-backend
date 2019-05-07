module Database.Migration.Development.Questionnaire.Data.Questionnaires where

import Control.Lens ((^.))
import Data.List
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import Database.Migration.Development.FilledKnowledgeModel.Data.FilledAnswersAndFollowUpQuestions
import Database.Migration.Development.FilledKnowledgeModel.Data.FilledChapters
import Database.Migration.Development.FilledKnowledgeModel.Data.FilledQuestions
import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireReply

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
  { _questionnaireUuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
  , _questionnaireName = "My Questionnaire"
  , _questionnaireLevel = 2
  , _questionnairePrivate = True
  , _questionnairePackageId = germanyPackage ^. pId
  , _questionnaireSelectedTagUuids = []
  , _questionnaireReplies =
      [ fQ1
      , fQ2
      , rQ2_aYes_fuQ1
      , fQ3
      , rQ4
      , rQ4_it1_itemName
      , rQ4_it1_q5
      , rQ4_it1_q5_it1_itemName
      , rQ4_it1_q5_it1_question7
      , rQ4_it1_q5_it1_question8
      , rQ4_it1_q6
      , rQ4_it2_itemName
      , rQ4_it2_q5
      , rQ4_it2_q6
      , rQ9
      , rQ10
      ]
  , _questionnaireOwnerUuid = Just $ fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66")
  , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

questionnaire1Edited :: Questionnaire
questionnaire1Edited =
  Questionnaire
  { _questionnaireUuid = questionnaire1 ^. uuid
  , _questionnaireName = "EDITED" ++ (questionnaire1 ^. name)
  , _questionnaireLevel = 3
  , _questionnairePrivate = False
  , _questionnairePackageId = questionnaire1 ^. packageId
  , _questionnaireSelectedTagUuids = questionnaire1 ^. selectedTagUuids
  , _questionnaireReplies = [fQ1, fQ2]
  , _questionnaireOwnerUuid = questionnaire1 ^. ownerUuid
  , _questionnaireCreatedAt = questionnaire1 ^. createdAt
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

createReplyKey :: [String] -> String
createReplyKey uuids = intercalate "." uuids

fQ1 :: Reply
fQ1 =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter1 ^. uuid, U.toString $ fQuestion1 ^. uuid]
  , _replyValue = StringReply . fromJust $ fQuestion1 ^. answerValue
  }

fQ2 :: Reply
fQ2 =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter1 ^. uuid, U.toString $ fQuestion2 ^. uuid]
  , _replyValue = AnswerReply $ (fromJust $ fQuestion2 ^. answerOption) ^. uuid
  }

rQ2_aYes_fuQ1 :: Reply
rQ2_aYes_fuQ1 =
  Reply
  { _replyPath =
      createReplyKey
        [ U.toString $ fChapter1 ^. uuid
        , U.toString $ fQuestion2 ^. uuid
        , U.toString $ fQ2_answerYes ^. uuid
        , U.toString $ fQ2_aYes_fuQuestion1 ^. uuid
        ]
  , _replyValue = AnswerReply $ (fromJust $ fQ2_aYes_fuQuestion1 ^. answerOption) ^. uuid
  }

fQ3 :: Reply
fQ3 =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion3 ^. uuid]
  , _replyValue = AnswerReply $ (fromJust $ fQuestion3 ^. answerOption) ^. uuid
  }

-- ------------------------------------------------------------
rQ4 :: Reply
rQ4 =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid]
  , _replyValue = ItemListReply 2
  }

rQ4_it1_itemName :: Reply
rQ4_it1_itemName =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "0", "itemName"]
  , _replyValue = StringReply . fromJust $ fQ4_ai1 ^. value
  }

rQ4_it1_q5 :: Reply
rQ4_it1_q5 =
  Reply
  { _replyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "0", U.toString $ fQ4_it1_question5 ^. uuid]
  , _replyValue = ItemListReply 1
  }

rQ4_it1_q5_it1_itemName :: Reply
rQ4_it1_q5_it1_itemName =
  Reply
  { _replyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "0"
        , U.toString $ fQ4_it1_question5 ^. uuid
        , "0"
        , "itemName"
        ]
  , _replyValue = StringReply . fromJust $ fQ4_it1_q5_ai1 ^. value
  }

rQ4_it1_q5_it1_question7 :: Reply
rQ4_it1_q5_it1_question7 =
  Reply
  { _replyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "0"
        , U.toString $ fQ4_it1_question5 ^. uuid
        , "0"
        , U.toString $ fQ4_it1_q5_it1_question7 ^. uuid
        ]
  , _replyValue = StringReply . fromJust $ fQ4_it1_q5_it1_question7 ^. answerValue
  }

rQ4_it1_q5_it1_question8 :: Reply
rQ4_it1_q5_it1_question8 =
  Reply
  { _replyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "0"
        , U.toString $ fQ4_it1_question5 ^. uuid
        , "0"
        , U.toString $ fQ4_it1_q5_it1_question8 ^. uuid
        ]
  , _replyValue = StringReply . fromJust $ fQ4_it1_q5_it1_question8 ^. answerValue
  }

rQ4_it1_q6 :: Reply
rQ4_it1_q6 =
  Reply
  { _replyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "0", U.toString $ fQ4_it1_question6 ^. uuid]
  , _replyValue = AnswerReply $ (fromJust (fQ4_it2_question6 ^. answerOption)) ^. uuid
  }

-- ------------------------------------------------------------
rQ4_it2_itemName :: Reply
rQ4_it2_itemName =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "1", "itemName"]
  , _replyValue = StringReply . fromJust $ fQ4_ai2 ^. value
  }

rQ4_it2_q5 :: Reply
rQ4_it2_q5 =
  Reply
  { _replyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "1", U.toString $ fQ4_it2_question5 ^. uuid]
  , _replyValue = ItemListReply 0
  }

rQ4_it2_q6 :: Reply
rQ4_it2_q6 =
  Reply
  { _replyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "1", U.toString $ fQ4_it2_question6 ^. uuid]
  , _replyValue = AnswerReply $ (fromJust $ fQ4_it2_question6 ^. answerOption) ^. uuid
  }

-- ------------------------------------------------------------
rQ9 :: Reply
rQ9 =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter3 ^. uuid, U.toString $ fQuestion9 ^. uuid]
  , _replyValue = IntegrationReply {_integrationReplyValue = PlainValue (fromJust $ fQuestion9 ^. answerValue)}
  }

rQ10 :: Reply
rQ10 =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter3 ^. uuid, U.toString $ fQuestion10 ^. uuid]
  , _replyValue =
      IntegrationReply
      { _integrationReplyValue =
          IntegrationValue
          { _integrationValueIntId = fromJust $ fQuestion10 ^. answerIntId
          , _integrationValueIntValue = fromJust $ fQuestion10 ^. answerValue
          }
      }
  }
