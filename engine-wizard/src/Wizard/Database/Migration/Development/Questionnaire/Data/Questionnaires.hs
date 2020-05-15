module Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires where

import Control.Lens ((^.))
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Questionnaire.QuestionnaireUtil
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireLabel
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper
import qualified Wizard.Service.User.UserMapper as U_Mapper

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
    , _questionnaireName = "My Private Questionnaire"
    , _questionnaireLevel = 1
    , _questionnaireAccessibility = PrivateQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateUuid = Just $ commonWizardTemplate ^. uuid
    , _questionnaireFormatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _questionnaireReplies = fReplies
    , _questionnaireLabels = fLabels
    , _questionnaireOwnerUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
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
    , _questionnaireTemplateUuid = Just $ commonWizardTemplate ^. uuid
    , _questionnaireFormatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _questionnaireReplies = questionnaire1 ^. replies
    , _questionnaireLabels = fLabelsEdited
    , _questionnaireOwnerUuid = Nothing
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatedAt = questionnaire1 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire1 ^. updatedAt
    }

questionnaire1Dto :: QuestionnaireDTO
questionnaire1Dto = toSimpleDTO questionnaire1 germanyPackage QSDefault (Just . U_Mapper.toDTO $ userAlbert)

questionnaire1Create :: QuestionnaireCreateDTO
questionnaire1Create =
  QuestionnaireCreateDTO
    { _questionnaireCreateDTOName = questionnaire1 ^. name
    , _questionnaireCreateDTOPackageId = questionnaire1 ^. packageId
    , _questionnaireCreateDTOAccessibility = questionnaire1 ^. accessibility
    , _questionnaireCreateDTOTagUuids = []
    , _questionnaireCreateDTOTemplateUuid = questionnaire1 ^. templateUuid
    }

questionnaire1EditedChange :: QuestionnaireChangeDTO
questionnaire1EditedChange =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName = questionnaire1Edited ^. name
    , _questionnaireChangeDTOAccessibility = questionnaire1Edited ^. accessibility
    , _questionnaireChangeDTOLevel = 1
    , _questionnaireChangeDTOReplies =
        toReplyDTO <$>
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
        ]
    , _questionnaireChangeDTOLabels = []
    , _questionnaireChangeDTOTemplateUuid = Nothing
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire2 :: Questionnaire
questionnaire2 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "d57520b4-5a70-4d40-8623-af2bfbbdfdfe")
    , _questionnaireName = "My PublicReadOnly Questionnaire"
    , _questionnaireLevel = questionnaire1 ^. level
    , _questionnaireAccessibility = PublicReadOnlyQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateUuid = Just $ commonWizardTemplate ^. uuid
    , _questionnaireFormatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _questionnaireReplies = fReplies
    , _questionnaireLabels = fLabels
    , _questionnaireOwnerUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
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
    , _questionnaireTemplateUuid = Just $ commonWizardTemplate ^. uuid
    , _questionnaireFormatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _questionnaireReplies = questionnaire2 ^. replies
    , _questionnaireLabels = fLabelsEdited
    , _questionnaireOwnerUuid = Nothing
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatedAt = questionnaire2 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire2 ^. updatedAt
    }

questionnaire2Dto :: QuestionnaireDTO
questionnaire2Dto = toSimpleDTO questionnaire2 germanyPackage QSDefault (Just . U_Mapper.toDTO $ userAlbert)

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire3 :: Questionnaire
questionnaire3 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "16530a07-e673-4ff3-ac1f-57250f2c1bfe")
    , _questionnaireName = "My Public Questionnaire"
    , _questionnaireLevel = questionnaire1 ^. level
    , _questionnaireAccessibility = PublicQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateUuid = Just $ commonWizardTemplate ^. uuid
    , _questionnaireFormatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _questionnaireReplies = fReplies
    , _questionnaireLabels = fLabels
    , _questionnaireOwnerUuid = Nothing
    , _questionnaireCreatorUuid = Nothing
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
    , _questionnaireTemplateUuid = Just $ commonWizardTemplate ^. uuid
    , _questionnaireFormatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _questionnaireReplies = questionnaire3 ^. replies
    , _questionnaireLabels = fLabelsEdited
    , _questionnaireOwnerUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatorUuid = Nothing
    , _questionnaireCreatedAt = questionnaire3 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire3 ^. updatedAt
    }

questionnaire3Dto :: QuestionnaireDTO
questionnaire3Dto = toSimpleDTO questionnaire3 germanyPackage QSDefault Nothing

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
    , _questionnaireTemplateUuid = Just $ commonWizardTemplate ^. uuid
    , _questionnaireFormatUuid = Just $ head (commonWizardTemplate ^. formats) ^. uuid
    , _questionnaireReplies = []
    , _questionnaireLabels = []
    , _questionnaireOwnerUuid = Nothing
    , _questionnaireCreatorUuid = Nothing
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
  , rQ4_it1_q5
  , rQ4_it1_q5_it1_question7
  , rQ4_it1_q5_it1_question8
  , rQ4_it1_q6
  , rQ4_it2_q5
  , rQ4_it2_q6
  , rQ9
  , rQ10
  ]

rQ1 :: Reply
rQ1 =
  Reply
    { _replyPath = createReplyKey [U.toString $ chapter1 ^. uuid, U.toString $ question1 ^. uuid]
    , _replyValue = StringReply "Reply to 1st question"
    }

rQ2 :: Reply
rQ2 =
  Reply
    { _replyPath = createReplyKey [U.toString $ chapter1 ^. uuid, U.toString $ question2 ^. uuid]
    , _replyValue = AnswerReply $ q2_answerYes ^. uuid
    }

rQ2_aYes_fuQ1 :: Reply
rQ2_aYes_fuQ1 =
  Reply
    { _replyPath =
        createReplyKey
          [ U.toString $ chapter1 ^. uuid
          , U.toString $ question2 ^. uuid
          , U.toString $ q2_answerYes ^. uuid
          , U.toString $ q2_aYes_fuQuestion1 ^. uuid
          ]
    , _replyValue = AnswerReply $ q2_aYes_fuq1_answerNo ^. uuid
    }

rQ3 :: Reply
rQ3 =
  Reply
    { _replyPath = createReplyKey [U.toString $ chapter2 ^. uuid, U.toString $ question3 ^. uuid]
    , _replyValue = AnswerReply $ q3_answerNo ^. uuid
    }

-- ------------------------------------------------------------
rQ4 :: Reply
rQ4 =
  Reply
    { _replyPath = createReplyKey [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid]
    , _replyValue = ItemListReply 2
    }

rQ4_it1_q5 :: Reply
rQ4_it1_q5 =
  Reply
    { _replyPath =
        createReplyKey
          [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid, "0", U.toString $ q4_it1_question5 ^. uuid]
    , _replyValue = ItemListReply 1
    }

rQ4_it1_q5_it1_question7 :: Reply
rQ4_it1_q5_it1_question7 =
  Reply
    { _replyPath =
        createReplyKey
          [ U.toString $ chapter2 ^. uuid
          , U.toString $ question4 ^. uuid
          , "0"
          , U.toString $ q4_it1_question5 ^. uuid
          , "0"
          , U.toString $ q4_it1_q5_it2_question7 ^. uuid
          ]
    , _replyValue = StringReply "Ai1: q5: Ai1: Reply to 7th question"
    }

rQ4_it1_q5_it1_question8 :: Reply
rQ4_it1_q5_it1_question8 =
  Reply
    { _replyPath =
        createReplyKey
          [ U.toString $ chapter2 ^. uuid
          , U.toString $ question4 ^. uuid
          , "0"
          , U.toString $ q4_it1_question5 ^. uuid
          , "0"
          , U.toString $ q4_it1_q5_it2_question8 ^. uuid
          ]
    , _replyValue = StringReply "Ai1: q5: Ai1: Reply to 8th question"
    }

rQ4_it1_q6 :: Reply
rQ4_it1_q6 =
  Reply
    { _replyPath =
        createReplyKey
          [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid, "0", U.toString $ q4_it1_question6 ^. uuid]
    , _replyValue = AnswerReply $ q4_it1_q6_answerNo ^. uuid
    }

-- ------------------------------------------------------------
rQ4_it2_q5 :: Reply
rQ4_it2_q5 =
  Reply
    { _replyPath =
        createReplyKey
          [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid, "1", U.toString $ q4_it1_question5 ^. uuid]
    , _replyValue = ItemListReply 0
    }

rQ4_it2_q6 :: Reply
rQ4_it2_q6 =
  Reply
    { _replyPath =
        createReplyKey
          [U.toString $ chapter2 ^. uuid, U.toString $ question4 ^. uuid, "1", U.toString $ q4_it1_question6 ^. uuid]
    , _replyValue = AnswerReply $ q4_it1_q6_answerNo ^. uuid
    }

-- ------------------------------------------------------------
rQ9 :: Reply
rQ9 =
  Reply
    { _replyPath = createReplyKey [U.toString $ chapter3 ^. uuid, U.toString $ question9 ^. uuid]
    , _replyValue = IntegrationReply {_integrationReplyValue = PlainValue "Plain reply to 9st question"}
    }

rQ10 :: Reply
rQ10 =
  Reply
    { _replyPath = createReplyKey [U.toString $ chapter3 ^. uuid, U.toString $ question10 ^. uuid]
    , _replyValue =
        IntegrationReply
          { _integrationReplyValue =
              IntegrationValue
                { _integrationValueIntId = "bsg-p000007"
                , _integrationValueIntValue = "Integration reply to 9st question"
                }
          }
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
fLabels :: [Label]
fLabels =
  [Label {_labelPath = rQ1 ^. path, _labelValue = [fromJust (U.fromString "3268ae3b-8c1a-44ea-ba69-ad759b3ef2ae")]}]

fLabelsEdited :: [Label]
fLabelsEdited =
  [ Label {_labelPath = rQ1 ^. path, _labelValue = [fromJust (U.fromString "3268ae3b-8c1a-44ea-ba69-ad759b3ef2ae")]}
  , Label {_labelPath = rQ2 ^. path, _labelValue = [fromJust (U.fromString "3268ae3b-8c1a-44ea-ba69-ad759b3ef2ae")]}
  ]
