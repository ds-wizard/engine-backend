module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents where

import Control.Lens ((^.), (^?), _Just)
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

fEvents :: [QuestionnaireEvent]
fEvents = fEventsWithoutComments ++ [ace_rQ1_t1_1', ace_rQ1_t1_2']

fEventsWithoutComments :: [QuestionnaireEvent]
fEventsWithoutComments =
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

fEventsWithoutCommentsDto :: [QuestionnaireEventDTO]
fEventsWithoutCommentsDto = fmap (\event -> toEventDTO event (Just userAlbert)) fEventsWithoutComments

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
    { _setReplyEventUuid = u' "8738f46e-0249-439e-95d9-12bc42247314"
    , _setReplyEventPath = fst rQ1
    , _setReplyEventValue = snd rQ1 ^. value
    , _setReplyEventCreatedBy = snd rQ1 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ1 ^. createdAt
    }

sre_rQ1Updated' :: QuestionnaireEvent
sre_rQ1Updated' = SetReplyEvent' sre_rQ1Updated

sre_rQ1Updated :: SetReplyEvent
sre_rQ1Updated =
  SetReplyEvent
    { _setReplyEventUuid = u' "88487886-ae2c-4820-9162-ede0aa4d6c5a"
    , _setReplyEventPath = fst rQ1Updated
    , _setReplyEventValue = snd rQ1Updated ^. value
    , _setReplyEventCreatedBy = snd rQ1Updated ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ1Updated ^. createdAt
    }

sre_rQ1Dto' :: QuestionnaireEventDTO
sre_rQ1Dto' = toEventDTO sre_rQ1' (Just userAlbert)

sre_rQ2' :: QuestionnaireEvent
sre_rQ2' = SetReplyEvent' sre_rQ2

sre_rQ2 :: SetReplyEvent
sre_rQ2 =
  SetReplyEvent
    { _setReplyEventUuid = u' "381e1c54-99f4-443a-a702-5dc60233046e"
    , _setReplyEventPath = fst rQ2
    , _setReplyEventValue = snd rQ2 ^. value
    , _setReplyEventCreatedBy = snd rQ2 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ2 ^. createdAt
    }

sre_rQ2_aYes_fuQ1' :: QuestionnaireEvent
sre_rQ2_aYes_fuQ1' = SetReplyEvent' sre_rQ2_aYes_fuQ1

sre_rQ2_aYes_fuQ1 :: SetReplyEvent
sre_rQ2_aYes_fuQ1 =
  SetReplyEvent
    { _setReplyEventUuid = u' "80cf2212-8064-4268-8478-c4db3ec5fadd"
    , _setReplyEventPath = fst rQ2_aYes_fuQ1
    , _setReplyEventValue = snd rQ2_aYes_fuQ1 ^. value
    , _setReplyEventCreatedBy = snd rQ2_aYes_fuQ1 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ2_aYes_fuQ1 ^. createdAt
    }

sre_rQ3' :: QuestionnaireEvent
sre_rQ3' = SetReplyEvent' sre_rQ3

sre_rQ3 :: SetReplyEvent
sre_rQ3 =
  SetReplyEvent
    { _setReplyEventUuid = u' "71b938b8-3a48-4335-a851-cf42760517d7"
    , _setReplyEventPath = fst rQ3
    , _setReplyEventValue = snd rQ3 ^. value
    , _setReplyEventCreatedBy = snd rQ3 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ3 ^. createdAt
    }

sre_rQ4' :: QuestionnaireEvent
sre_rQ4' = SetReplyEvent' sre_rQ4

sre_rQ4 :: SetReplyEvent
sre_rQ4 =
  SetReplyEvent
    { _setReplyEventUuid = u' "aa342fec-7e05-450b-b7ec-52d6816a471c"
    , _setReplyEventPath = fst rQ4
    , _setReplyEventValue = snd rQ4 ^. value
    , _setReplyEventCreatedBy = snd rQ4 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ4 ^. createdAt
    }

sre_rQ4_it1_q5' :: QuestionnaireEvent
sre_rQ4_it1_q5' = SetReplyEvent' sre_rQ4_it1_q5

sre_rQ4_it1_q5 :: SetReplyEvent
sre_rQ4_it1_q5 =
  SetReplyEvent
    { _setReplyEventUuid = u' "06c84eb0-966f-49e6-806e-51954a9feb0b"
    , _setReplyEventPath = fst rQ4_it1_q5
    , _setReplyEventValue = snd rQ4_it1_q5 ^. value
    , _setReplyEventCreatedBy = snd rQ4_it1_q5 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ4_it1_q5 ^. createdAt
    }

sre_rQ4_it1_q5_it1_question7' :: QuestionnaireEvent
sre_rQ4_it1_q5_it1_question7' = SetReplyEvent' sre_rQ4_it1_q5_it1_question7

sre_rQ4_it1_q5_it1_question7 :: SetReplyEvent
sre_rQ4_it1_q5_it1_question7 =
  SetReplyEvent
    { _setReplyEventUuid = u' "3b352af1-4cee-403a-86e2-7927b71393bc"
    , _setReplyEventPath = fst rQ4_it1_q5_it1_question7
    , _setReplyEventValue = snd rQ4_it1_q5_it1_question7 ^. value
    , _setReplyEventCreatedBy = snd rQ4_it1_q5_it1_question7 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ4_it1_q5_it1_question7 ^. createdAt
    }

sre_rQ4_it1_q5_it1_question8' :: QuestionnaireEvent
sre_rQ4_it1_q5_it1_question8' = SetReplyEvent' sre_rQ4_it1_q5_it1_question8

sre_rQ4_it1_q5_it1_question8 :: SetReplyEvent
sre_rQ4_it1_q5_it1_question8 =
  SetReplyEvent
    { _setReplyEventUuid = u' "a5e38b38-3d50-4fd4-af8e-a8b3312ce8be"
    , _setReplyEventPath = fst rQ4_it1_q5_it1_question8
    , _setReplyEventValue = snd rQ4_it1_q5_it1_question8 ^. value
    , _setReplyEventCreatedBy = snd rQ4_it1_q5_it1_question8 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ4_it1_q5_it1_question8 ^. createdAt
    }

sre_rQ4_it1_q6' :: QuestionnaireEvent
sre_rQ4_it1_q6' = SetReplyEvent' sre_rQ4_it1_q6

sre_rQ4_it1_q6 :: SetReplyEvent
sre_rQ4_it1_q6 =
  SetReplyEvent
    { _setReplyEventUuid = u' "5fbe670c-5ea9-420d-a495-d270a461c2bb"
    , _setReplyEventPath = fst rQ4_it1_q6
    , _setReplyEventValue = snd rQ4_it1_q6 ^. value
    , _setReplyEventCreatedBy = snd rQ4_it1_q6 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ4_it1_q6 ^. createdAt
    }

sre_rQ4_it2_q5' :: QuestionnaireEvent
sre_rQ4_it2_q5' = SetReplyEvent' sre_rQ4_it2_q5

sre_rQ4_it2_q5 :: SetReplyEvent
sre_rQ4_it2_q5 =
  SetReplyEvent
    { _setReplyEventUuid = u' "cc09e14f-305c-4d26-98eb-e30b5086cea2"
    , _setReplyEventPath = fst rQ4_it2_q5
    , _setReplyEventValue = snd rQ4_it2_q5 ^. value
    , _setReplyEventCreatedBy = snd rQ4_it2_q5 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ4_it2_q5 ^. createdAt
    }

sre_rQ4_it2_q6' :: QuestionnaireEvent
sre_rQ4_it2_q6' = SetReplyEvent' sre_rQ4_it2_q6

sre_rQ4_it2_q6 :: SetReplyEvent
sre_rQ4_it2_q6 =
  SetReplyEvent
    { _setReplyEventUuid = u' "2682fa2e-bd54-4cb3-a23b-8a83232defb2"
    , _setReplyEventPath = fst rQ4_it2_q6
    , _setReplyEventValue = snd rQ4_it2_q6 ^. value
    , _setReplyEventCreatedBy = snd rQ4_it2_q6 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ4_it2_q6 ^. createdAt
    }

sre_rQ9' :: QuestionnaireEvent
sre_rQ9' = SetReplyEvent' sre_rQ9

sre_rQ9 :: SetReplyEvent
sre_rQ9 =
  SetReplyEvent
    { _setReplyEventUuid = u' "ce19fd6a-669c-4f62-a868-064a51dfe89a"
    , _setReplyEventPath = fst rQ9
    , _setReplyEventValue = snd rQ9 ^. value
    , _setReplyEventCreatedBy = snd rQ9 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ9 ^. createdAt
    }

sre_rQ10' :: QuestionnaireEvent
sre_rQ10' = SetReplyEvent' sre_rQ10

sre_rQ10 :: SetReplyEvent
sre_rQ10 =
  SetReplyEvent
    { _setReplyEventUuid = u' "9e75ab96-3721-4eb5-adcb-40df71a81b92"
    , _setReplyEventPath = fst rQ10
    , _setReplyEventValue = snd rQ10 ^. value
    , _setReplyEventCreatedBy = snd rQ10 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ10 ^. createdAt
    }

sre_rQ11' :: QuestionnaireEvent
sre_rQ11' = SetReplyEvent' sre_rQ11

sre_rQ11 :: SetReplyEvent
sre_rQ11 =
  SetReplyEvent
    { _setReplyEventUuid = u' "3658de32-5033-4fa4-a89b-c4f4481d5670"
    , _setReplyEventPath = fst rQ11
    , _setReplyEventValue = snd rQ11 ^. value
    , _setReplyEventCreatedBy = snd rQ11 ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = snd rQ11 ^. createdAt
    }

cre_rQ1' :: QuestionnaireEvent
cre_rQ1' = ClearReplyEvent' cre_rQ1

cre_rQ1 :: ClearReplyEvent
cre_rQ1 =
  ClearReplyEvent
    { _clearReplyEventUuid = u' "1c9c9b1b-ba64-438e-a415-c513e14de55e"
    , _clearReplyEventPath = fst rQ1
    , _clearReplyEventCreatedBy = Just $ userAlbert ^. uuid
    , _clearReplyEventCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

sphse_1' :: QuestionnaireEvent
sphse_1' = SetPhaseEvent' sphse_1

sphse_1 :: SetPhaseEvent
sphse_1 =
  SetPhaseEvent
    { _setPhaseEventUuid = u' "f5288ec0-16b4-4a22-8c4f-ee411f0005d3"
    , _setPhaseEventPhaseUuid = Just $ phase1 ^. uuid
    , _setPhaseEventCreatedBy = Just $ userAlbert ^. uuid
    , _setPhaseEventCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

sphse_2' :: QuestionnaireEvent
sphse_2' = SetPhaseEvent' sphse_2

sphse_2 :: SetPhaseEvent
sphse_2 =
  SetPhaseEvent
    { _setPhaseEventUuid = u' "3bbd961e-eace-4b74-967e-43eae0986894"
    , _setPhaseEventPhaseUuid = Just $ phase2 ^. uuid
    , _setPhaseEventCreatedBy = Just $ userAlbert ^. uuid
    , _setPhaseEventCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

sphse_3' :: QuestionnaireEvent
sphse_3' = SetPhaseEvent' sphse_3

sphse_3 :: SetPhaseEvent
sphse_3 =
  SetPhaseEvent
    { _setPhaseEventUuid = u' "c6c68cce-015d-4c6f-adba-dacc8f77de05"
    , _setPhaseEventPhaseUuid = Just $ phase3 ^. uuid
    , _setPhaseEventCreatedBy = Just $ userAlbert ^. uuid
    , _setPhaseEventCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

slble_rQ1' :: QuestionnaireEvent
slble_rQ1' = SetLabelsEvent' slble_rQ1

slble_rQ1 :: SetLabelsEvent
slble_rQ1 =
  SetLabelsEvent
    { _setLabelsEventUuid = u' "530c93da-0af5-42eb-970e-dd016270ce7e"
    , _setLabelsEventPath = fst rQ1
    , _setLabelsEventValue = [fLabel1]
    , _setLabelsEventCreatedBy = Just $ userAlbert ^. uuid
    , _setLabelsEventCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

slble_rQ2' :: QuestionnaireEvent
slble_rQ2' = SetLabelsEvent' slble_rQ2

slble_rQ2 :: SetLabelsEvent
slble_rQ2 =
  SetLabelsEvent
    { _setLabelsEventUuid = u' "91a574db-1b8b-444d-a8e6-e2acc52bf8db"
    , _setLabelsEventPath = fst rQ2
    , _setLabelsEventValue = [fLabel1]
    , _setLabelsEventCreatedBy = Just $ userAlbert ^. uuid
    , _setLabelsEventCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

rte_rQ1_t1' :: QuestionnaireEvent
rte_rQ1_t1' = ResolveCommentThreadEvent' rte_rQ1_t1

rte_rQ1_t1 :: ResolveCommentThreadEvent
rte_rQ1_t1 =
  ResolveCommentThreadEvent
    { _resolveCommentThreadEventUuid = u' "ad5ffe15-d895-4452-af31-3b952db0b8a8"
    , _resolveCommentThreadEventPath = cmtQ1_path
    , _resolveCommentThreadEventThreadUuid = cmtQ1_t1 ^. uuid
    , _resolveCommentThreadEventCreatedBy = cmtQ1_t1Resolved ^. createdBy ^? _Just . uuid
    , _resolveCommentThreadEventCreatedAt = cmtQ1_t1Resolved ^. createdAt
    }

ote_rQ1_t1' :: QuestionnaireEvent
ote_rQ1_t1' = ReopenCommentThreadEvent' ote_rQ1_t1

ote_rQ1_t1 :: ReopenCommentThreadEvent
ote_rQ1_t1 =
  ReopenCommentThreadEvent
    { _reopenCommentThreadEventUuid = u' "444c89c8-ead9-44c7-9621-0c0c43ff5f9f"
    , _reopenCommentThreadEventPath = cmtQ1_path
    , _reopenCommentThreadEventThreadUuid = cmtQ1_t1 ^. uuid
    , _reopenCommentThreadEventCreatedBy = cmtQ1_t1 ^. createdBy ^? _Just . uuid
    , _reopenCommentThreadEventCreatedAt = cmtQ1_t1 ^. createdAt
    }

dte_rQ1_t1' :: QuestionnaireEvent
dte_rQ1_t1' = DeleteCommentThreadEvent' dte_rQ1_t1

dte_rQ1_t1 :: DeleteCommentThreadEvent
dte_rQ1_t1 =
  DeleteCommentThreadEvent
    { _deleteCommentThreadEventUuid = u' "0e8a5812-90da-43b1-bb20-dbf8a95aa00d"
    , _deleteCommentThreadEventPath = cmtQ1_path
    , _deleteCommentThreadEventThreadUuid = cmtQ1_t1 ^. uuid
    , _deleteCommentThreadEventCreatedBy = cmtQ1_t1 ^. createdBy ^? _Just . uuid
    , _deleteCommentThreadEventCreatedAt = cmtQ1_t1 ^. createdAt
    }

ace_rQ1_t1_1' :: QuestionnaireEvent
ace_rQ1_t1_1' = AddCommentEvent' ace_rQ1_t1_1

ace_rQ1_t1_1 :: AddCommentEvent
ace_rQ1_t1_1 =
  AddCommentEvent
    { _addCommentEventUuid = u' "471ef7d8-5164-44ba-9d28-8aad036458fd"
    , _addCommentEventPath = cmtQ1_path
    , _addCommentEventThreadUuid = cmtQ1_t1 ^. uuid
    , _addCommentEventCommentUuid = cmtQ1_t1_1 ^. uuid
    , _addCommentEventText = cmtQ1_t1_1 ^. text
    , _addCommentEventPrivate = cmtQ1_t1 ^. private
    , _addCommentEventCreatedBy = cmtQ1_t1_1 ^. createdBy ^? _Just . uuid
    , _addCommentEventCreatedAt = cmtQ1_t1_1 ^. createdAt
    }

ace_rQ1_t1_2' :: QuestionnaireEvent
ace_rQ1_t1_2' = AddCommentEvent' ace_rQ1_t1_2

ace_rQ1_t1_2 :: AddCommentEvent
ace_rQ1_t1_2 =
  AddCommentEvent
    { _addCommentEventUuid = u' "3450c5ca-a267-4be0-b112-92f5c0d2e2a8"
    , _addCommentEventPath = cmtQ1_path
    , _addCommentEventThreadUuid = cmtQ1_t1 ^. uuid
    , _addCommentEventCommentUuid = cmtQ1_t1_2 ^. uuid
    , _addCommentEventText = cmtQ1_t1_2 ^. text
    , _addCommentEventPrivate = cmtQ1_t1 ^. private
    , _addCommentEventCreatedBy = cmtQ1_t1_2 ^. createdBy ^? _Just . uuid
    , _addCommentEventCreatedAt = cmtQ1_t1_2 ^. createdAt
    }

ace_rQ2_t1_1' :: QuestionnaireEvent
ace_rQ2_t1_1' = AddCommentEvent' ace_rQ2_t1_1

ace_rQ2_t1_1 :: AddCommentEvent
ace_rQ2_t1_1 =
  AddCommentEvent
    { _addCommentEventUuid = u' "b46c4ff7-8af2-4164-8e94-841b4f8c312b"
    , _addCommentEventPath = cmtQ2_path
    , _addCommentEventThreadUuid = cmtQ2_t1 ^. uuid
    , _addCommentEventCommentUuid = cmtQ2_t1_1 ^. uuid
    , _addCommentEventText = cmtQ2_t1_1 ^. text
    , _addCommentEventPrivate = cmtQ2_t1 ^. private
    , _addCommentEventCreatedBy = cmtQ2_t1_1 ^. createdBy ^? _Just . uuid
    , _addCommentEventCreatedAt = cmtQ2_t1_1 ^. createdAt
    }

ece_rQ1_t1_1' :: QuestionnaireEvent
ece_rQ1_t1_1' = EditCommentEvent' ece_rQ1_t1_1

ece_rQ1_t1_1 :: EditCommentEvent
ece_rQ1_t1_1 =
  EditCommentEvent
    { _editCommentEventUuid = u' "6a598663-4bce-48e9-83d0-422ae753f60d"
    , _editCommentEventPath = cmtQ1_path
    , _editCommentEventThreadUuid = cmtQ1_t1 ^. uuid
    , _editCommentEventCommentUuid = cmtQ1_t1_1Edited ^. uuid
    , _editCommentEventText = cmtQ1_t1_1Edited ^. text
    , _editCommentEventCreatedBy = cmtQ1_t1_1Edited ^. createdBy ^? _Just . uuid
    , _editCommentEventCreatedAt = cmtQ1_t1_1Edited ^. updatedAt
    }

dce_rQ1_t1_1' :: QuestionnaireEvent
dce_rQ1_t1_1' = DeleteCommentEvent' dce_rQ1_t1_1

dce_rQ1_t1_1 :: DeleteCommentEvent
dce_rQ1_t1_1 =
  DeleteCommentEvent
    { _deleteCommentEventUuid = u' "0e8a5812-90da-43b1-bb20-dbf8a95aa00d"
    , _deleteCommentEventPath = cmtQ1_path
    , _deleteCommentEventThreadUuid = cmtQ1_t1 ^. uuid
    , _deleteCommentEventCommentUuid = cmtQ1_t1_1 ^. uuid
    , _deleteCommentEventCreatedBy = cmtQ1_t1_1 ^. createdBy ^? _Just . uuid
    , _deleteCommentEventCreatedAt = cmtQ1_t1_1 ^. createdAt
    }
