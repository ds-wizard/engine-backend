module Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Questionnaire.QuestionnaireUtil
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
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
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
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
    , _questionnaireName = "EDITED: " ++ (questionnaire1 ^. name)
    , _questionnaireLevel = questionnaire1 ^. level
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = questionnaire1 ^. packageId
    , _questionnaireSelectedTagUuids = questionnaire1 ^. selectedTagUuids
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireReplies = questionnaire1 ^. replies
    , _questionnaireLabels = questionnaire1 ^. labels
    , _questionnaireOwnerUuid = Nothing
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatedAt = questionnaire1 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire1 ^. updatedAt
    }

questionnaire1ContentEdited :: Questionnaire
questionnaire1ContentEdited =
  questionnaire1 {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

questionnaire1Dto :: QuestionnaireDTO
questionnaire1Dto =
  toSimpleDTO questionnaire1 germanyPackage QSDefault (Just . U_Mapper.toDTO $ userAlbert) questionnaireReport

questionnaire1Create :: QuestionnaireCreateDTO
questionnaire1Create =
  QuestionnaireCreateDTO
    { _questionnaireCreateDTOName = questionnaire1 ^. name
    , _questionnaireCreateDTOPackageId = questionnaire1 ^. packageId
    , _questionnaireCreateDTOVisibility = questionnaire1 ^. visibility
    , _questionnaireCreateDTOSharing = questionnaire1 ^. sharing
    , _questionnaireCreateDTOTagUuids = []
    , _questionnaireCreateDTOTemplateId = questionnaire1 ^. templateId
    , _questionnaireCreateDTOFormatUuid = questionnaire1 ^. formatUuid
    }

questionnaire1EditedChange :: QuestionnaireChangeDTO
questionnaire1EditedChange =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName = questionnaire1Edited ^. name
    , _questionnaireChangeDTOVisibility = questionnaire1Edited ^. visibility
    , _questionnaireChangeDTOSharing = questionnaire1Edited ^. sharing
    , _questionnaireChangeDTOTemplateId = Nothing
    , _questionnaireChangeDTOFormatUuid = Nothing
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire2 :: Questionnaire
questionnaire2 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "d57520b4-5a70-4d40-8623-af2bfbbdfdfe")
    , _questionnaireName = "My VisibleView Questionnaire"
    , _questionnaireLevel = questionnaire1 ^. level
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireReplies = fReplies
    , _questionnaireLabels = fLabels
    , _questionnaireOwnerUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    }

questionnaire2Edited :: Questionnaire
questionnaire2Edited =
  Questionnaire
    { _questionnaireUuid = questionnaire2 ^. uuid
    , _questionnaireName = "EDITED: " ++ (questionnaire2 ^. name)
    , _questionnaireLevel = questionnaire2 ^. level
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = questionnaire2 ^. packageId
    , _questionnaireSelectedTagUuids = questionnaire2 ^. selectedTagUuids
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireReplies = questionnaire2 ^. replies
    , _questionnaireLabels = questionnaire2 ^. labels
    , _questionnaireOwnerUuid = Nothing
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatedAt = questionnaire2 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire2 ^. updatedAt
    }

questionnaire2ContentEdited :: Questionnaire
questionnaire2ContentEdited =
  questionnaire2 {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

questionnaire2Dto :: QuestionnaireDTO
questionnaire2Dto =
  toSimpleDTO questionnaire2 germanyPackage QSDefault (Just . U_Mapper.toDTO $ userAlbert) questionnaireReport

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire3 :: Questionnaire
questionnaire3 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "16530a07-e673-4ff3-ac1f-57250f2c1bfe")
    , _questionnaireName = "My VisibleEdit Questionnaire"
    , _questionnaireLevel = questionnaire1 ^. level
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireReplies = fReplies
    , _questionnaireLabels = fLabels
    , _questionnaireOwnerUuid = Nothing
    , _questionnaireCreatorUuid = Nothing
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 28) 0
    }

questionnaire3Edited :: Questionnaire
questionnaire3Edited =
  Questionnaire
    { _questionnaireUuid = questionnaire3 ^. uuid
    , _questionnaireName = "EDITED: " ++ (questionnaire3 ^. name)
    , _questionnaireLevel = questionnaire3 ^. level
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = questionnaire3 ^. packageId
    , _questionnaireSelectedTagUuids = questionnaire3 ^. selectedTagUuids
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireReplies = questionnaire3 ^. replies
    , _questionnaireLabels = questionnaire3 ^. labels
    , _questionnaireOwnerUuid = Just $ userAlbert ^. uuid
    , _questionnaireCreatorUuid = Nothing
    , _questionnaireCreatedAt = questionnaire3 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire3 ^. updatedAt
    }

questionnaire3ContentEdited :: Questionnaire
questionnaire3ContentEdited =
  questionnaire1 {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

questionnaire3Dto :: QuestionnaireDTO
questionnaire3Dto = toSimpleDTO questionnaire3 germanyPackage QSDefault Nothing questionnaireReport

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire4 :: Questionnaire
questionnaire4 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "57250a07-a663-4ff3-ac1f-16530f2c1bfe")
    , _questionnaireName = "Outdated Questionnaire"
    , _questionnaireLevel = 2
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = netherlandsPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireReplies = M.empty
    , _questionnaireLabels = M.empty
    , _questionnaireOwnerUuid = Nothing
    , _questionnaireCreatorUuid = Nothing
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire4VisibleView :: Questionnaire
questionnaire4VisibleView = questionnaire4 {_questionnaireVisibility = VisibleViewQuestionnaire}

questionnaire4VisibleEdit :: Questionnaire
questionnaire4VisibleEdit =
  questionnaire4 {_questionnaireVisibility = VisibleEditQuestionnaire, _questionnaireOwnerUuid = Nothing}

questionnaire4Upgraded :: Questionnaire
questionnaire4Upgraded =
  questionnaire4
    { _questionnaireUuid = fromJust (U.fromString "5deabef8-f526-421c-90e2-dd7aed1a25c5")
    , _questionnairePackageId = netherlandsPackageV2 ^. pId
    }

questionnaire4VisibleViewUpgraded :: Questionnaire
questionnaire4VisibleViewUpgraded = questionnaire4Upgraded {_questionnaireVisibility = VisibleViewQuestionnaire}

questionnaire4VisibleEditUpgraded :: Questionnaire
questionnaire4VisibleEditUpgraded =
  questionnaire4Upgraded {_questionnaireVisibility = VisibleEditQuestionnaire, _questionnaireOwnerUuid = Nothing}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire5 :: Questionnaire
questionnaire5 =
  questionnaire1
    { _questionnaireUuid = fromJust (U.fromString "506be867-ba92-4e10-8175-187e99613366")
    , _questionnaireName = "My Private Questionnaire SharedView"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkViewQuestionnaire
    }

questionnaire5ContentEdited :: Questionnaire
questionnaire5ContentEdited =
  questionnaire5 {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire6 :: Questionnaire
questionnaire6 =
  questionnaire1
    { _questionnaireUuid = fromJust (U.fromString "09304abd-2035-4046-8dc8-b3e5ba8c016c")
    , _questionnaireName = "My Private Questionnaire SharedEdit"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire6ContentEdited :: Questionnaire
questionnaire6ContentEdited =
  questionnaire6 {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

questionnaire6Dto :: QuestionnaireDTO
questionnaire6Dto =
  toSimpleDTO questionnaire6 germanyPackage QSDefault (Just . U_Mapper.toDTO $ userAlbert) questionnaireReport

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire7 :: Questionnaire
questionnaire7 =
  questionnaire2
    { _questionnaireUuid = fromJust (U.fromString "abd22b10-63fd-4cb8-bb23-7997ff32eccc")
    , _questionnaireName = "My VisibleView Questionnaire SharedView"
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkViewQuestionnaire
    }

questionnaire7ContentEdited :: Questionnaire
questionnaire7ContentEdited =
  questionnaire7 {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire8 :: Questionnaire
questionnaire8 =
  questionnaire2
    { _questionnaireUuid = fromJust (U.fromString "a990f62a-ca1f-4517-82d4-399951b8630b")
    , _questionnaireName = "My VisibleView Questionnaire SharedEdit"
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire8ContentEdited :: Questionnaire
questionnaire8ContentEdited =
  questionnaire8 {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire9 :: Questionnaire
questionnaire9 =
  questionnaire2
    { _questionnaireUuid = fromJust (U.fromString "936e852f-4c41-4524-8387-bd87090e9fcc")
    , _questionnaireName = "My VisibleEdit Questionnaire SharedView"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkViewQuestionnaire
    }

questionnaire9ContentEdited :: Questionnaire
questionnaire9ContentEdited =
  questionnaire9 {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire10 :: Questionnaire
questionnaire10 =
  questionnaire3
    { _questionnaireUuid = fromJust (U.fromString "3c8e7ce6-cb5e-4cd1-a4d1-fb9de55f67ed")
    , _questionnaireName = "My VisibleEdit Questionnaire SharedEdit"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire10ContentEdited :: Questionnaire
questionnaire10ContentEdited =
  questionnaire10
    {_questionnaireLevel = 3, _questionnaireReplies = fRepliesEdited, _questionnaireLabels = fLabelsEdited}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
contentChangeDTO :: QuestionnaireContentChangeDTO
contentChangeDTO =
  QuestionnaireContentChangeDTO
    { _questionnaireContentChangeDTOLevel = 1
    , _questionnaireContentChangeDTOReplies = M.map toReplyValueDTO fReplies
    , _questionnaireContentChangeDTOLabels = fLabels
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
fReplies :: M.Map String ReplyValue
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
    ]

fRepliesWithUpdated :: M.Map String ReplyValue
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
    ]

fRepliesWithDeleted :: M.Map String ReplyValue
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
    ]

fRepliesEdited :: M.Map String ReplyValue
fRepliesEdited = M.fromList [rQ1, rQ2, rQ2_aYes_fuQ1, rQ3, rQ9, rQ10]

rQ1 :: Reply
rQ1 = (createReplyKey [chapter1 ^. uuid, question1 ^. uuid], StringReply "Reply to 1st question")

rQ1Updated :: Reply
rQ1Updated = (createReplyKey [chapter1 ^. uuid, question1 ^. uuid], StringReply "Updated Reply to 1st question")

rQ2 :: Reply
rQ2 = (createReplyKey [chapter1 ^. uuid, question2 ^. uuid], AnswerReply $ q2_answerYes ^. uuid)

rQ2_aYes_fuQ1 :: Reply
rQ2_aYes_fuQ1 =
  ( createReplyKey [chapter1 ^. uuid, question2 ^. uuid, q2_answerYes ^. uuid, q2_aYes_fuQuestion1 ^. uuid]
  , AnswerReply $ q2_aYes_fuq1_answerNo ^. uuid)

unused_rQ2_aYes_fuQ1_aYes_fuq2 :: Reply
unused_rQ2_aYes_fuQ1_aYes_fuq2 =
  ( createReplyKey
      [ chapter1 ^. uuid
      , question2 ^. uuid
      , q2_answerYes ^. uuid
      , q2_aYes_fuQuestion1 ^. uuid
      , q2_aYes_fuq1_answerYes ^. uuid
      , q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
      ]
  , AnswerReply $ q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid)

rQ3 :: Reply
rQ3 = (createReplyKey [chapter2 ^. uuid, question3 ^. uuid], AnswerReply $ q3_answerNo ^. uuid)

-- ------------------------------------------------------------
rQ4 :: Reply
rQ4 = (createReplyKey [chapter2 ^. uuid, question4 ^. uuid], ItemListReply [rQ4_it1, rQ4_it2])

rQ4_it1 :: U.UUID
rQ4_it1 = u' "97e42df3-f0f6-40f8-83ab-375a1340e8ab"

rQ4_it1_q5 :: Reply
rQ4_it1_q5 =
  ( createReplyKey [chapter2 ^. uuid, question4 ^. uuid, rQ4_it1, q4_it1_question5 ^. uuid]
  , ItemListReply [rQ4_it1_q5_it1])

rQ4_it1_q5_it1 :: U.UUID
rQ4_it1_q5_it1 = u' "12c243e0-f300-4178-ae4e-0b30d01c6f73"

rQ4_it1_q5_it1_question7 :: Reply
rQ4_it1_q5_it1_question7 =
  ( createReplyKey
      [ chapter2 ^. uuid
      , question4 ^. uuid
      , rQ4_it1
      , q4_it1_question5 ^. uuid
      , rQ4_it1_q5_it1
      , q4_it1_q5_it2_question7 ^. uuid
      ]
  , StringReply "Ai1: q5: Ai1: Reply to 7th question")

rQ4_it1_q5_it1_question8 :: Reply
rQ4_it1_q5_it1_question8 =
  ( createReplyKey
      [ chapter2 ^. uuid
      , question4 ^. uuid
      , rQ4_it1
      , q4_it1_question5 ^. uuid
      , rQ4_it1_q5_it1
      , q4_it1_q5_it2_question8 ^. uuid
      ]
  , StringReply "Ai1: q5: Ai1: Reply to 8th question")

rQ4_it1_q6 :: Reply
rQ4_it1_q6 =
  ( createReplyKey [chapter2 ^. uuid, question4 ^. uuid, rQ4_it1, q4_it1_question6 ^. uuid]
  , AnswerReply $ q4_it1_q6_answerNo ^. uuid)

-- ------------------------------------------------------------
rQ4_it2 :: U.UUID
rQ4_it2 = u' "aed4bcbc-0c63-4bf6-b954-4561e92babfa"

rQ4_it2_q5 :: Reply
rQ4_it2_q5 = (createReplyKey [chapter2 ^. uuid, question4 ^. uuid, rQ4_it2, q4_it1_question5 ^. uuid], ItemListReply [])

rQ4_it2_q6 :: Reply
rQ4_it2_q6 =
  ( createReplyKey [chapter2 ^. uuid, question4 ^. uuid, rQ4_it2, q4_it1_question6 ^. uuid]
  , AnswerReply $ q4_it1_q6_answerNo ^. uuid)

-- ------------------------------------------------------------
rQ9 :: Reply
rQ9 =
  ( createReplyKey [chapter3 ^. uuid, question9 ^. uuid]
  , IntegrationReply {_integrationReplyValue = PlainValue "Plain reply to 9st question"})

rQ10 :: Reply
rQ10 =
  ( createReplyKey [chapter3 ^. uuid, question10 ^. uuid]
  , IntegrationReply
      { _integrationReplyValue =
          IntegrationValue
            {_integrationValueIntId = "bsg-p000007", _integrationValueValue = "Integration reply to 9st question"}
      })

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
fLabel1 = u' "3268ae3b-8c1a-44ea-ba69-ad759b3ef2ae"

fLabels :: M.Map String [U.UUID]
fLabels = M.fromList [(fst rQ1, [fLabel1])]

fLabelsEdited :: M.Map String [U.UUID]
fLabelsEdited = M.fromList [(fst rQ1, [fLabel1]), (fst rQ2, [fLabel1])]
