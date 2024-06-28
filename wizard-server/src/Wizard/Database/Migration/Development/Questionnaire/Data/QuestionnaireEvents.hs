module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.User.User
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.User.UserMapper
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

fEvents :: [QuestionnaireEvent]
fEvents =
  [ sre_rQ1'
  , sre_rQ2'
  , sre_rQ2_aYes_fuQ1'
  , sre_rQ3'
  , sre_rQ4'
  , sre_rQ4_it1_q5'
  , sre_rQ4_it1_q5_it1_question7'
  , sre_rQ4_it1_q5_it1_question8'
  , sre_rQ4_it1_q6'
  , sre_rQ4_it2_q5'
  , sre_rQ4_it2_q6'
  , sre_rQ9'
  , sre_rQ10'
  , sre_rQ11'
  , sphse_1'
  , slble_rQ1'
  ]

fEventsDto :: [QuestionnaireEventDTO]
fEventsDto = fmap (\event -> toEventDTO event (Just userAlbert)) fEvents

fEventsWithUpdated :: [QuestionnaireEvent]
fEventsWithUpdated = fEvents ++ [sre_rQ1Updated']

fEventsWithDeleted :: [QuestionnaireEvent]
fEventsWithDeleted = fEvents ++ [cre_rQ1']

fEventsEdited :: [QuestionnaireEvent]
fEventsEdited = fEvents ++ [slble_rQ2']

sre_rQ1' :: QuestionnaireEvent
sre_rQ1' = SetReplyEvent' sre_rQ1

sre_rQ1 :: SetReplyEvent
sre_rQ1 =
  SetReplyEvent
    { uuid = u' "8738f46e-0249-439e-95d9-12bc42247314"
    , path = fst rQ1
    , value = (snd rQ1).value
    , createdBy = fmap (.uuid) $ (snd rQ1).createdBy
    , createdAt = (snd rQ1).createdAt
    }

sre_rQ1Updated' :: QuestionnaireEvent
sre_rQ1Updated' = SetReplyEvent' sre_rQ1Updated

sre_rQ1Updated :: SetReplyEvent
sre_rQ1Updated =
  SetReplyEvent
    { uuid = u' "88487886-ae2c-4820-9162-ede0aa4d6c5a"
    , path = fst rQ1Updated
    , value = (snd rQ1Updated).value
    , createdBy = fmap (.uuid) $ (snd rQ1Updated).createdBy
    , createdAt = (snd rQ1Updated).createdAt
    }

sre_rQ1Dto' :: QuestionnaireEventDTO
sre_rQ1Dto' = toEventDTO sre_rQ1' (Just userAlbert)

sre_rQ2' :: QuestionnaireEvent
sre_rQ2' = SetReplyEvent' sre_rQ2

sre_rQ2 :: SetReplyEvent
sre_rQ2 =
  SetReplyEvent
    { uuid = u' "381e1c54-99f4-443a-a702-5dc60233046e"
    , path = fst rQ2
    , value = (snd rQ2).value
    , createdBy = fmap (.uuid) $ (snd rQ2).createdBy
    , createdAt = (snd rQ2).createdAt
    }

sre_rQ2_aYes_fuQ1' :: QuestionnaireEvent
sre_rQ2_aYes_fuQ1' = SetReplyEvent' sre_rQ2_aYes_fuQ1

sre_rQ2_aYes_fuQ1 :: SetReplyEvent
sre_rQ2_aYes_fuQ1 =
  SetReplyEvent
    { uuid = u' "80cf2212-8064-4268-8478-c4db3ec5fadd"
    , path = fst rQ2_aYes_fuQ1
    , value = (snd rQ2_aYes_fuQ1).value
    , createdBy = fmap (.uuid) $ (snd rQ2_aYes_fuQ1).createdBy
    , createdAt = (snd rQ2_aYes_fuQ1).createdAt
    }

sre_rQ3' :: QuestionnaireEvent
sre_rQ3' = SetReplyEvent' sre_rQ3

sre_rQ3 :: SetReplyEvent
sre_rQ3 =
  SetReplyEvent
    { uuid = u' "71b938b8-3a48-4335-a851-cf42760517d7"
    , path = fst rQ3
    , value = (snd rQ3).value
    , createdBy = fmap (.uuid) $ (snd rQ3).createdBy
    , createdAt = (snd rQ3).createdAt
    }

sre_rQ4' :: QuestionnaireEvent
sre_rQ4' = SetReplyEvent' sre_rQ4

sre_rQ4 :: SetReplyEvent
sre_rQ4 =
  SetReplyEvent
    { uuid = u' "aa342fec-7e05-450b-b7ec-52d6816a471c"
    , path = fst rQ4
    , value = (snd rQ4).value
    , createdBy = fmap (.uuid) $ (snd rQ4).createdBy
    , createdAt = (snd rQ4).createdAt
    }

sre_rQ4_it1_q5' :: QuestionnaireEvent
sre_rQ4_it1_q5' = SetReplyEvent' sre_rQ4_it1_q5

sre_rQ4_it1_q5 :: SetReplyEvent
sre_rQ4_it1_q5 =
  SetReplyEvent
    { uuid = u' "06c84eb0-966f-49e6-806e-51954a9feb0b"
    , path = fst rQ4_it1_q5
    , value = (snd rQ4_it1_q5).value
    , createdBy = fmap (.uuid) $ (snd rQ4_it1_q5).createdBy
    , createdAt = (snd rQ4_it1_q5).createdAt
    }

sre_rQ4_it1_q5_it1_question7' :: QuestionnaireEvent
sre_rQ4_it1_q5_it1_question7' = SetReplyEvent' sre_rQ4_it1_q5_it1_question7

sre_rQ4_it1_q5_it1_question7 :: SetReplyEvent
sre_rQ4_it1_q5_it1_question7 =
  SetReplyEvent
    { uuid = u' "3b352af1-4cee-403a-86e2-7927b71393bc"
    , path = fst rQ4_it1_q5_it1_question7
    , value = (snd rQ4_it1_q5_it1_question7).value
    , createdBy = fmap (.uuid) $ (snd rQ4_it1_q5_it1_question7).createdBy
    , createdAt = (snd rQ4_it1_q5_it1_question7).createdAt
    }

sre_rQ4_it1_q5_it1_question8' :: QuestionnaireEvent
sre_rQ4_it1_q5_it1_question8' = SetReplyEvent' sre_rQ4_it1_q5_it1_question8

sre_rQ4_it1_q5_it1_question8 :: SetReplyEvent
sre_rQ4_it1_q5_it1_question8 =
  SetReplyEvent
    { uuid = u' "a5e38b38-3d50-4fd4-af8e-a8b3312ce8be"
    , path = fst rQ4_it1_q5_it1_question8
    , value = (snd rQ4_it1_q5_it1_question8).value
    , createdBy = fmap (.uuid) $ (snd rQ4_it1_q5_it1_question8).createdBy
    , createdAt = (snd rQ4_it1_q5_it1_question8).createdAt
    }

sre_rQ4_it1_q6' :: QuestionnaireEvent
sre_rQ4_it1_q6' = SetReplyEvent' sre_rQ4_it1_q6

sre_rQ4_it1_q6 :: SetReplyEvent
sre_rQ4_it1_q6 =
  SetReplyEvent
    { uuid = u' "5fbe670c-5ea9-420d-a495-d270a461c2bb"
    , path = fst rQ4_it1_q6
    , value = (snd rQ4_it1_q6).value
    , createdBy = fmap (.uuid) $ (snd rQ4_it1_q6).createdBy
    , createdAt = (snd rQ4_it1_q6).createdAt
    }

sre_rQ4_it2_q5' :: QuestionnaireEvent
sre_rQ4_it2_q5' = SetReplyEvent' sre_rQ4_it2_q5

sre_rQ4_it2_q5 :: SetReplyEvent
sre_rQ4_it2_q5 =
  SetReplyEvent
    { uuid = u' "cc09e14f-305c-4d26-98eb-e30b5086cea2"
    , path = fst rQ4_it2_q5
    , value = (snd rQ4_it2_q5).value
    , createdBy = fmap (.uuid) $ (snd rQ4_it2_q5).createdBy
    , createdAt = (snd rQ4_it2_q5).createdAt
    }

sre_rQ4_it2_q6' :: QuestionnaireEvent
sre_rQ4_it2_q6' = SetReplyEvent' sre_rQ4_it2_q6

sre_rQ4_it2_q6 :: SetReplyEvent
sre_rQ4_it2_q6 =
  SetReplyEvent
    { uuid = u' "2682fa2e-bd54-4cb3-a23b-8a83232defb2"
    , path = fst rQ4_it2_q6
    , value = (snd rQ4_it2_q6).value
    , createdBy = fmap (.uuid) $ (snd rQ4_it2_q6).createdBy
    , createdAt = (snd rQ4_it2_q6).createdAt
    }

sre_rQ9' :: QuestionnaireEvent
sre_rQ9' = SetReplyEvent' sre_rQ9

sre_rQ9 :: SetReplyEvent
sre_rQ9 =
  SetReplyEvent
    { uuid = u' "ce19fd6a-669c-4f62-a868-064a51dfe89a"
    , path = fst rQ9
    , value = (snd rQ9).value
    , createdBy = fmap (.uuid) $ (snd rQ9).createdBy
    , createdAt = (snd rQ9).createdAt
    }

sre_rQ10' :: QuestionnaireEvent
sre_rQ10' = SetReplyEvent' sre_rQ10

sre_rQ10 :: SetReplyEvent
sre_rQ10 =
  SetReplyEvent
    { uuid = u' "9e75ab96-3721-4eb5-adcb-40df71a81b92"
    , path = fst rQ10
    , value = (snd rQ10).value
    , createdBy = fmap (.uuid) $ (snd rQ10).createdBy
    , createdAt = (snd rQ10).createdAt
    }

sre_rQ11' :: QuestionnaireEvent
sre_rQ11' = SetReplyEvent' sre_rQ11

sre_rQ11 :: SetReplyEvent
sre_rQ11 =
  SetReplyEvent
    { uuid = u' "3658de32-5033-4fa4-a89b-c4f4481d5670"
    , path = fst rQ11
    , value = (snd rQ11).value
    , createdBy = fmap (.uuid) $ (snd rQ11).createdBy
    , createdAt = (snd rQ11).createdAt
    }

cre_rQ1' :: QuestionnaireEvent
cre_rQ1' = ClearReplyEvent' cre_rQ1

cre_rQ1 :: ClearReplyEvent
cre_rQ1 =
  ClearReplyEvent
    { uuid = u' "1c9c9b1b-ba64-438e-a415-c513e14de55e"
    , path = fst rQ1
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

sphse_1' :: QuestionnaireEvent
sphse_1' = SetPhaseEvent' sphse_1

sphse_1 :: SetPhaseEvent
sphse_1 =
  SetPhaseEvent
    { uuid = u' "f5288ec0-16b4-4a22-8c4f-ee411f0005d3"
    , phaseUuid = Just $ phase1.uuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

sphse_2' :: QuestionnaireEvent
sphse_2' = SetPhaseEvent' sphse_2

sphse_2 :: SetPhaseEvent
sphse_2 =
  SetPhaseEvent
    { uuid = u' "3bbd961e-eace-4b74-967e-43eae0986894"
    , phaseUuid = Just $ phase2.uuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

sphse_3' :: QuestionnaireEvent
sphse_3' = SetPhaseEvent' sphse_3

sphse_3 :: SetPhaseEvent
sphse_3 =
  SetPhaseEvent
    { uuid = u' "c6c68cce-015d-4c6f-adba-dacc8f77de05"
    , phaseUuid = Just $ phase3.uuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

slble_rQ1' :: QuestionnaireEvent
slble_rQ1' = SetLabelsEvent' slble_rQ1

slble_rQ1 :: SetLabelsEvent
slble_rQ1 =
  SetLabelsEvent
    { uuid = u' "530c93da-0af5-42eb-970e-dd016270ce7e"
    , path = fst rQ1
    , value = [fLabel1]
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

slble_rQ2' :: QuestionnaireEvent
slble_rQ2' = SetLabelsEvent' slble_rQ2

slble_rQ2 :: SetLabelsEvent
slble_rQ2 =
  SetLabelsEvent
    { uuid = u' "91a574db-1b8b-444d-a8e6-e2acc52bf8db"
    , path = fst rQ2
    , value = [fLabel1]
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

rte_rQ1_t1' :: QuestionnaireEventDTO
rte_rQ1_t1' = ResolveCommentThreadEventDTO' rte_rQ1_t1

rte_rQ1_t1 :: ResolveCommentThreadEventDTO
rte_rQ1_t1 =
  ResolveCommentThreadEventDTO
    { uuid = u' "ad5ffe15-d895-4452-af31-3b952db0b8a8"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentCount = 1
    , createdBy = Just . toSuggestionDTO . toSuggestion $ userAlbert
    , createdAt = cmtQ1_t1Resolved.createdAt
    }

rtche_rQ1_t1' :: QuestionnaireEventChangeDTO
rtche_rQ1_t1' = ResolveCommentThreadEventChangeDTO' rtche_rQ1_t1

rtche_rQ1_t1 :: ResolveCommentThreadEventChangeDTO
rtche_rQ1_t1 =
  ResolveCommentThreadEventChangeDTO
    { uuid = rte_rQ1_t1.uuid
    , path = rte_rQ1_t1.path
    , threadUuid = rte_rQ1_t1.threadUuid
    , private = False
    , commentCount = 1
    }

ote_rQ1_t1' :: QuestionnaireEventDTO
ote_rQ1_t1' = ReopenCommentThreadEventDTO' ote_rQ1_t1

ote_rQ1_t1 :: ReopenCommentThreadEventDTO
ote_rQ1_t1 =
  ReopenCommentThreadEventDTO
    { uuid = u' "444c89c8-ead9-44c7-9621-0c0c43ff5f9f"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentCount = 1
    , createdBy = Just . toSuggestionDTO . toSuggestion $ userAlbert
    , createdAt = cmtQ1_t1.createdAt
    }

otche_rQ1_t1' :: QuestionnaireEventChangeDTO
otche_rQ1_t1' = ReopenCommentThreadEventChangeDTO' otche_rQ1_t1

otche_rQ1_t1 :: ReopenCommentThreadEventChangeDTO
otche_rQ1_t1 =
  ReopenCommentThreadEventChangeDTO
    { uuid = ote_rQ1_t1.uuid
    , path = ote_rQ1_t1.path
    , threadUuid = ote_rQ1_t1.threadUuid
    , commentCount = 1
    , private = False
    }

dte_rQ1_t1' :: QuestionnaireEventDTO
dte_rQ1_t1' = DeleteCommentThreadEventDTO' dte_rQ1_t1

dte_rQ1_t1 :: DeleteCommentThreadEventDTO
dte_rQ1_t1 =
  DeleteCommentThreadEventDTO
    { uuid = u' "0e8a5812-90da-43b1-bb20-dbf8a95aa00d"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , createdBy = Just . toSuggestionDTO . toSuggestion $ userAlbert
    , createdAt = cmtQ1_t1.createdAt
    }

dtche_rQ1_t1' :: QuestionnaireEventChangeDTO
dtche_rQ1_t1' = DeleteCommentThreadEventChangeDTO' dtche_rQ1_t1

dtche_rQ1_t1 :: DeleteCommentThreadEventChangeDTO
dtche_rQ1_t1 =
  DeleteCommentThreadEventChangeDTO
    { uuid = dte_rQ1_t1.uuid
    , path = dte_rQ1_t1.path
    , threadUuid = dte_rQ1_t1.threadUuid
    , private = False
    }

ace_rQ1_t1_1' :: QuestionnaireEventDTO
ace_rQ1_t1_1' = AddCommentEventDTO' ace_rQ1_t1_1

ace_rQ1_t1_1 :: AddCommentEventDTO
ace_rQ1_t1_1 =
  AddCommentEventDTO
    { uuid = u' "471ef7d8-5164-44ba-9d28-8aad036458fd"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentUuid = cmtQ1_t1_1.uuid
    , text = cmtQ1_t1_1.text
    , private = cmtQ1_t1.private
    , createdBy = Just . toSuggestionDTO . toSuggestion $ userAlbert
    , createdAt = cmtQ1_t1_1.createdAt
    }

ace_rQ1_t1_2' :: QuestionnaireEventDTO
ace_rQ1_t1_2' = AddCommentEventDTO' ace_rQ1_t1_2

ace_rQ1_t1_2 :: AddCommentEventDTO
ace_rQ1_t1_2 =
  AddCommentEventDTO
    { uuid = u' "3450c5ca-a267-4be0-b112-92f5c0d2e2a8"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentUuid = cmtQ1_t1_2.uuid
    , text = cmtQ1_t1_2.text
    , private = cmtQ1_t1.private
    , createdBy = Just . toSuggestionDTO . toSuggestion $ userAlbert
    , createdAt = cmtQ1_t1_2.createdAt
    }

ace_rQ2_t1_1' :: QuestionnaireEventDTO
ace_rQ2_t1_1' = AddCommentEventDTO' ace_rQ2_t1_1

ace_rQ2_t1_1 :: AddCommentEventDTO
ace_rQ2_t1_1 =
  AddCommentEventDTO
    { uuid = u' "b46c4ff7-8af2-4164-8e94-841b4f8c312b"
    , path = cmtQ2_path
    , threadUuid = cmtQ2_t1.uuid
    , commentUuid = cmtQ2_t1_1.uuid
    , text = cmtQ2_t1_1.text
    , private = cmtQ2_t1.private
    , createdBy = Just . toSuggestionDTO . toSuggestion $ userAlbert
    , createdAt = cmtQ2_t1_1.createdAt
    }

acche_rQ2_t1_1' :: QuestionnaireEventChangeDTO
acche_rQ2_t1_1' = AddCommentEventChangeDTO' acche_rQ2_t1_1

acche_rQ2_t1_1 :: AddCommentEventChangeDTO
acche_rQ2_t1_1 =
  AddCommentEventChangeDTO
    { uuid = ace_rQ2_t1_1.uuid
    , path = ace_rQ2_t1_1.path
    , threadUuid = ace_rQ2_t1_1.threadUuid
    , commentUuid = ace_rQ2_t1_1.commentUuid
    , text = ace_rQ2_t1_1.text
    , private = False
    , newThread = True
    }

ece_rQ1_t1_1' :: QuestionnaireEventDTO
ece_rQ1_t1_1' = EditCommentEventDTO' ece_rQ1_t1_1

ece_rQ1_t1_1 :: EditCommentEventDTO
ece_rQ1_t1_1 =
  EditCommentEventDTO
    { uuid = u' "6a598663-4bce-48e9-83d0-422ae753f60d"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentUuid = cmtQ1_t1_1Edited.uuid
    , text = cmtQ1_t1_1Edited.text
    , createdBy = Just . toSuggestionDTO . toSuggestion $ userAlbert
    , createdAt = cmtQ1_t1_1Edited.updatedAt
    }

ecche_rQ1_t1_1' :: QuestionnaireEventChangeDTO
ecche_rQ1_t1_1' = EditCommentEventChangeDTO' ecche_rQ1_t1_1

ecche_rQ1_t1_1 :: EditCommentEventChangeDTO
ecche_rQ1_t1_1 =
  EditCommentEventChangeDTO
    { uuid = ece_rQ1_t1_1.uuid
    , path = ece_rQ1_t1_1.path
    , threadUuid = ece_rQ1_t1_1.threadUuid
    , commentUuid = ece_rQ1_t1_1.commentUuid
    , text = ece_rQ1_t1_1.text
    , private = False
    }

dce_rQ1_t1_1' :: QuestionnaireEventDTO
dce_rQ1_t1_1' = DeleteCommentEventDTO' dce_rQ1_t1_1

dce_rQ1_t1_1 :: DeleteCommentEventDTO
dce_rQ1_t1_1 =
  DeleteCommentEventDTO
    { uuid = u' "0e8a5812-90da-43b1-bb20-dbf8a95aa00d"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentUuid = cmtQ1_t1_1.uuid
    , createdBy = Just . toSuggestionDTO . toSuggestion $ userAlbert
    , createdAt = cmtQ1_t1_1.createdAt
    }

dcche_rQ1_t1_1' :: QuestionnaireEventChangeDTO
dcche_rQ1_t1_1' = DeleteCommentEventChangeDTO' dcche_rQ1_t1_1

dcche_rQ1_t1_1 :: DeleteCommentEventChangeDTO
dcche_rQ1_t1_1 =
  DeleteCommentEventChangeDTO
    { uuid = dce_rQ1_t1_1.uuid
    , path = dce_rQ1_t1_1.path
    , threadUuid = dce_rQ1_t1_1.threadUuid
    , commentUuid = dce_rQ1_t1_1.commentUuid
    , private = False
    }
