module Database.Migration.Questionnaire.Data.Questionnaires where

import Control.Lens ((^.))
import Data.List
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.FilledKnowledgeModel.Data.FilledAnswersAndFollowUpQuestions
import Database.Migration.FilledKnowledgeModel.Data.FilledChapters
import Database.Migration.FilledKnowledgeModel.Data.FilledQuestions
import LensesConfig
import Model.Questionnaire.Questionnaire

questionnaire1 =
  Questionnaire
  { _questionnaireUuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
  , _questionnaireName = "My Questionnaire"
  , _questionnairePackageId = "elixir.nl:core-nl:2.0.0"
  , _questionnaireKnowledgeModel = km1WithQ4
  , _questionnaireReplies =
      [ fQ1
      , fQ2
      , rQ2_aYes_fuQ1
      , fQ3
      , rQ4_ait1_itemName
      , rQ4_ait1_q5_ait1_itemName
      , rQ4_ait1_q5_ait1_question7
      , rQ4_ait1_q5_ait1_question8
      , rQ4_ait1_q6
      , rQ4_ait2_itemName
      , rQ4_ait2_q5_ait1_itemName
      , rQ4_ait2_q5_ait1_question7
      , rQ4_ait2_q5_ait1_question8
      , rQ4_ait2_q6
      ]
  , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
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
rQ4_ait1_itemName =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "0", "itemName"]
  , _questionnaireReplyValue = fQ4_ait1 ^. value
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
  , _questionnaireReplyValue = fQ4_ait1_q5_ait1 ^. value
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
  , _questionnaireReplyValue = fQ4_ait2 ^. value
  }

rQ4_ait2_q5_ait1_itemName =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "1"
        , U.toString $ fQ4_ait2_question5 ^. uuid
        , "0"
        , "itemName"
        ]
  , _questionnaireReplyValue = fQ4_ait2_q5_ait1 ^. value
  }

rQ4_ait2_q5_ait1_question7 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "1"
        , U.toString $ fQ4_ait2_question5 ^. uuid
        , "0"
        , U.toString $ fQ4_ait2_q5_ait1_question7 ^. uuid
        ]
  , _questionnaireReplyValue = fromJust (fQ4_ait2_q5_ait1_question7 ^. answerValue)
  }

rQ4_ait2_q5_ait1_question8 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [ U.toString $ fChapter2 ^. uuid
        , U.toString $ fQuestion4 ^. uuid
        , "1"
        , U.toString $ fQ4_ait2_question5 ^. uuid
        , "0"
        , U.toString $ fQ4_ait2_q5_ait1_question8 ^. uuid
        ]
  , _questionnaireReplyValue = fromJust (fQ4_ait2_q5_ait1_question8 ^. answerValue)
  }

rQ4_ait2_q6 =
  QuestionnaireReply
  { _questionnaireReplyPath =
      createReplyKey
        [U.toString $ fChapter2 ^. uuid, U.toString $ fQuestion4 ^. uuid, "1", U.toString $ fQ4_ait2_question6 ^. uuid]
  , _questionnaireReplyValue = U.toString $ (fromJust (fQ4_ait2_question6 ^. answerOption)) ^. uuid
  }
