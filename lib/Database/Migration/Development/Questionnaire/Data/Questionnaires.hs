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
import Database.Migration.Development.User.Data.Users
import LensesConfig
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireReply

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
  { _questionnaireUuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
  , _questionnaireName = "My Private Questionnaire"
  , _questionnaireLevel = 2
  , _questionnaireAccessibility = PrivateQuestionnaire
  , _questionnairePackageId = germanyPackage ^. pId
  , _questionnaireSelectedTagUuids = []
  , _questionnaireReplies = fReplies
  , _questionnaireOwnerUuid = Just $ userAlbert ^. uuid
  , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

questionnaire1Edited :: Questionnaire
questionnaire1Edited =
  Questionnaire
  { _questionnaireUuid = questionnaire1 ^. uuid
  , _questionnaireName = "EDITED" ++ (questionnaire1 ^. name)
  , _questionnaireLevel = 3
  , _questionnaireAccessibility = PublicQuestionnaire
  , _questionnairePackageId = questionnaire1 ^. packageId
  , _questionnaireSelectedTagUuids = questionnaire1 ^. selectedTagUuids
  , _questionnaireReplies = questionnaire1 ^. replies
  , _questionnaireOwnerUuid = Nothing
  , _questionnaireCreatedAt = questionnaire1 ^. createdAt
  , _questionnaireUpdatedAt = questionnaire1 ^. updatedAt
  }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire2 :: Questionnaire
questionnaire2 =
  Questionnaire
  { _questionnaireUuid = fromJust (U.fromString "d57520b4-5a70-4d40-8623-af2bfbbdfdfe")
  , _questionnaireName = "My PublicReadOnly Questionnaire"
  , _questionnaireLevel = 2
  , _questionnaireAccessibility = PublicReadOnlyQuestionnaire
  , _questionnairePackageId = germanyPackage ^. pId
  , _questionnaireSelectedTagUuids = []
  , _questionnaireReplies = fReplies
  , _questionnaireOwnerUuid = Just $ userAlbert ^. uuid
  , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

questionnaire2Edited :: Questionnaire
questionnaire2Edited =
  Questionnaire
  { _questionnaireUuid = questionnaire2 ^. uuid
  , _questionnaireName = "EDITED" ++ (questionnaire2 ^. name)
  , _questionnaireLevel = 3
  , _questionnaireAccessibility = PublicQuestionnaire
  , _questionnairePackageId = questionnaire2 ^. packageId
  , _questionnaireSelectedTagUuids = questionnaire2 ^. selectedTagUuids
  , _questionnaireReplies = questionnaire2 ^. replies
  , _questionnaireOwnerUuid = Nothing
  , _questionnaireCreatedAt = questionnaire2 ^. createdAt
  , _questionnaireUpdatedAt = questionnaire2 ^. updatedAt
  }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire3 :: Questionnaire
questionnaire3 =
  Questionnaire
  { _questionnaireUuid = fromJust (U.fromString "16530a07-e673-4ff3-ac1f-57250f2c1bfe")
  , _questionnaireName = "My Public Questionnaire"
  , _questionnaireLevel = 2
  , _questionnaireAccessibility = PublicQuestionnaire
  , _questionnairePackageId = germanyPackage ^. pId
  , _questionnaireSelectedTagUuids = []
  , _questionnaireReplies = fReplies
  , _questionnaireOwnerUuid = Nothing
  , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

questionnaire3Edited :: Questionnaire
questionnaire3Edited =
  Questionnaire
  { _questionnaireUuid = questionnaire3 ^. uuid
  , _questionnaireName = "EDITED" ++ (questionnaire3 ^. name)
  , _questionnaireLevel = 3
  , _questionnaireAccessibility = PrivateQuestionnaire
  , _questionnairePackageId = questionnaire3 ^. packageId
  , _questionnaireSelectedTagUuids = questionnaire3 ^. selectedTagUuids
  , _questionnaireReplies = questionnaire3 ^. replies
  , _questionnaireOwnerUuid = Just $ userAlbert ^. uuid
  , _questionnaireCreatedAt = questionnaire3 ^. createdAt
  , _questionnaireUpdatedAt = questionnaire3 ^. updatedAt
  }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire4 :: Questionnaire
questionnaire4 =
  Questionnaire
  { _questionnaireUuid = fromJust (U.fromString "57250a07-a663-4ff3-ac1f-16530f2c1bfe")
  , _questionnaireName = "Outdated Questionnaire"
  , _questionnaireLevel = 2
  , _questionnaireAccessibility = PrivateQuestionnaire
  , _questionnairePackageId = netherlandsPackage ^. pId
  , _questionnaireSelectedTagUuids = []
  , _questionnaireReplies = []
  , _questionnaireOwnerUuid = Nothing
  , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

questionnaire4PublicReadOnly :: Questionnaire
questionnaire4PublicReadOnly = questionnaire4 {_questionnaireAccessibility = PublicReadOnlyQuestionnaire}

questionnaire4Public :: Questionnaire
questionnaire4Public =
  questionnaire4 {_questionnaireAccessibility = PublicQuestionnaire, _questionnaireOwnerUuid = Nothing}

questionnaire4Upgraded :: Questionnaire
questionnaire4Upgraded =
  questionnaire4
  { _questionnaireUuid = fromJust (U.fromString "5deabef8-f526-421c-90e2-dd7aed1a25c5")
  , _questionnairePackageId = netherlandsPackageV2 ^. pId
  }

questionnaire4PublicReadOnlyUpgraded :: Questionnaire
questionnaire4PublicReadOnlyUpgraded =
  questionnaire4Upgraded {_questionnaireAccessibility = PublicReadOnlyQuestionnaire}

questionnaire4PublicUpgraded :: Questionnaire
questionnaire4PublicUpgraded =
  questionnaire4Upgraded {_questionnaireAccessibility = PublicQuestionnaire, _questionnaireOwnerUuid = Nothing}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
fReplies :: [Reply]
fReplies =
  [ rQ1
  , rQ2
  , rQ2_aYes_fuQ1
  , rQ3
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

createReplyKey :: [String] -> String
createReplyKey uuids = intercalate "." uuids

rQ1 :: Reply
rQ1 =
  Reply
  { _replyPath = createReplyKey [U.toString $ fChapter1 ^. uuid, U.toString $ fQuestion1 ^. uuid]
  , _replyValue = StringReply . fromJust $ fQuestion1 ^. answerValue
  }

rQ2 :: Reply
rQ2 =
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

rQ3 :: Reply
rQ3 =
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
