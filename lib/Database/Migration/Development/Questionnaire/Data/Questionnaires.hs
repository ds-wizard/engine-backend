module Database.Migration.Development.Questionnaire.Data.Questionnaires where

import Control.Lens ((^.))
import Data.List
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import Database.Migration.Development.FilledKnowledgeModel.Data.FilledAnswersAndFollowUpQuestions
import Database.Migration.Development.FilledKnowledgeModel.Data.FilledChapters
import Database.Migration.Development.FilledKnowledgeModel.Data.FilledQuestions
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import LensesConfig
import Model.Questionnaire.Questionnaire

questionnaire1 =
  Questionnaire
  { _questionnaireUuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
  , _questionnaireName = "My Questionnaire"
  , _questionnaireLevel = 2
  , _questionnairePrivate = True
  , _questionnairePackageId = "elixir.nl:core-nl:2.0.0"
  , _questionnaireKnowledgeModel = km1WithQ4
  , _questionnaireReplies =
      [ fQ1
      , fQ2
      , rQ2_aYes_fuQ1
      , fQ3
      , rQ4
      , rQ4_ait1_itemName
      , rQ4_ait1_q5
      , rQ4_ait1_q5_ait1_itemName
      , rQ4_ait1_q5_ait1_question7
      , rQ4_ait1_q5_ait1_question8
      , rQ4_ait1_q6
      , rQ4_ait2_itemName
      , rQ4_ait2_q5
      , rQ4_ait2_q6
      ]
  , _questionnaireOwnerUuid = Just $ fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66")
  , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

questionnaire1Changed =
  Questionnaire
  { _questionnaireUuid = questionnaire1 ^. uuid
  , _questionnaireName = questionnaire1 ^. name
  , _questionnaireLevel = 3
  , _questionnairePrivate = questionnaire1 ^. private
  , _questionnairePackageId = questionnaire1 ^. packageId
  , _questionnaireKnowledgeModel = questionnaire1 ^. knowledgeModel
  , _questionnaireReplies = [fQ1, fQ2]
  , _questionnaireOwnerUuid = questionnaire1 ^. ownerUuid
  , _questionnaireCreatedAt = questionnaire1 ^. createdAt
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

createReplyKey :: [String] -> String
createReplyKey uuids = intercalate "." uuids

fQ1 =
  QuestionnaireReply
  { _questionnaireReplyPath = createReplyKey [U.toString $ fChapter1 ^. uuid, U.toString $ fQuestion1 ^. uuid]
  , _questionnaireReplyValue = fromJust (fQuestion1 ^. answerValue)
  }

fQ2 =
  QuestionnaireReply
  { _questionnaireReplyPath = createReplyKey [U.toString $ fChapter1 ^. uuid, U.toString $ fQuestion2 ^. uuid]
  , _questionnaireReplyValue = U.toString $ (fromJust (fQuestion2 ^. answerOption)) ^. uuid
  }

rQ2_aYes_fuQ1 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [ U.toString $ fChapter1 ^. uuid
        , U.toString $ fQuestion2 ^. uuid
        , U.toString $ fQ2_answerYes ^. uuid
        , U.toString $ fQ2_aYes_fuQuestion1 ^. uuid
        ]
  , _questionnaireReplyValue = U.toString $ (fromJust (fQ2_aYes_fuQuestion1 ^. answerOption)) ^. uuid
  }

fQ3 =
  QuestionnaireReply
  { _questionnaireReplyPath = createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion3 ^. uuid]
  , _questionnaireReplyValue = U.toString $ (fromJust (fQuestion3 ^. answerOption)) ^. uuid
  }

-- ------------------------------------------------------------
rQ4 =
  QuestionnaireReply
  { _questionnaireReplyPath = createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid]
  , _questionnaireReplyValue = "2"
  }

rQ4_ait1_itemName =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "0", "itemName"]
  , _questionnaireReplyValue = fromJust $ fQ4_ait1 ^. value
  }

rQ4_ait1_q5 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "0", U.toString $ fQ4_ait1_question5 ^. uuid]
  , _questionnaireReplyValue = "1"
  }

rQ4_ait1_q5_ait1_itemName =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "0"
        , U.toString $ fQ4_ait1_question5 ^. uuid
        , "0"
        , "itemName"
        ]
  , _questionnaireReplyValue = fromJust $ fQ4_ait1_q5_ait1 ^. value
  }

rQ4_ait1_q5_ait1_question7 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "0"
        , U.toString $ fQ4_ait1_question5 ^. uuid
        , "0"
        , U.toString $ fQ4_ait1_q5_ait1_question7 ^. uuid
        ]
  , _questionnaireReplyValue = fromJust (fQ4_ait1_q5_ait1_question7 ^. answerValue)
  }

rQ4_ait1_q5_ait1_question8 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "0"
        , U.toString $ fQ4_ait1_question5 ^. uuid
        , "0"
        , U.toString $ fQ4_ait1_q5_ait1_question8 ^. uuid
        ]
  , _questionnaireReplyValue = fromJust (fQ4_ait1_q5_ait1_question8 ^. answerValue)
  }

rQ4_ait1_q6 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "0", U.toString $ fQ4_ait1_question6 ^. uuid]
  , _questionnaireReplyValue = U.toString $ (fromJust (fQ4_ait2_question6 ^. answerOption)) ^. uuid
  }

-- ------------------------------------------------------------
rQ4_ait2_itemName =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "1", "itemName"]
  , _questionnaireReplyValue = fromJust $ fQ4_ait2 ^. value
  }

rQ4_ait2_q5 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "1", U.toString $ fQ4_ait2_question5 ^. uuid]
  , _questionnaireReplyValue = "0"
  }

rQ4_ait2_q6 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "1", U.toString $ fQ4_ait2_question6 ^. uuid]
  , _questionnaireReplyValue = U.toString $ (fromJust (fQ4_ait2_question6 ^. answerOption)) ^. uuid
  }
