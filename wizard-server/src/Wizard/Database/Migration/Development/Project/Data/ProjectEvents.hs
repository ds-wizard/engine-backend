module Wizard.Database.Migration.Development.Project.Data.ProjectEvents where

import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.Tenant
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Api.Resource.Project.Event.ProjectEventDTO
import Wizard.Database.Migration.Development.Project.Data.ProjectComments
import Wizard.Database.Migration.Development.Project.Data.ProjectLabels
import Wizard.Database.Migration.Development.Project.Data.ProjectReplies
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Project.Comment.ProjectComment
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.ProjectReply
import Wizard.Model.User.User
import Wizard.Service.Project.Event.ProjectEventMapper
import Wizard.Service.User.UserMapper
import WizardLib.Public.Model.User.UserSuggestion

fEvents :: U.UUID -> [ProjectEvent]
fEvents projectUuid =
  [ sre_rQ1' projectUuid
  , sre_rQ2' projectUuid
  , sre_rQ2_aYes_fuQ1' projectUuid
  , sre_rQ3' projectUuid
  , sre_rQ4' projectUuid
  , sre_rQ4_it1_q5' projectUuid
  , sre_rQ4_it1_q5_it1_question7' projectUuid
  , sre_rQ4_it1_q5_it1_question8' projectUuid
  , sre_rQ4_it1_q6' projectUuid
  , sre_rQ4_it2_q5' projectUuid
  , sre_rQ4_it2_q6' projectUuid
  , sre_rQ9' projectUuid
  , sre_rQ10' projectUuid
  , sre_rQ11' projectUuid
  , sphse_1' projectUuid
  , slble_rQ1' projectUuid
  ]

fEventsDto :: U.UUID -> [ProjectEventDTO]
fEventsDto projectUuid = fmap (\event -> toEventDTO event (Just userAlbert)) (fEvents projectUuid)

fEventsList :: U.UUID -> [ProjectEventList]
fEventsList projectUuid = fmap (\event -> toEventList event (Just userAlbert)) (fEvents projectUuid)

fEventsWithUpdated :: U.UUID -> [ProjectEvent]
fEventsWithUpdated projectUuid = fEvents projectUuid ++ [sre_rQ1Updated' projectUuid]

fEventsWithDeleted :: U.UUID -> [ProjectEvent]
fEventsWithDeleted projectUuid = fEvents projectUuid ++ [cre_rQ1' projectUuid]

fEventsEdited :: U.UUID -> [ProjectEvent]
fEventsEdited projectUuid = fEvents projectUuid ++ [slble_rQ2' projectUuid]

sre_rQ1' :: U.UUID -> ProjectEvent
sre_rQ1' = SetReplyEvent' . sre_rQ1

sre_rQ1 :: U.UUID -> SetReplyEvent
sre_rQ1 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "12bc42247314"
    , path = fst rQ1
    , value = (snd rQ1).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ1).createdBy
    , createdAt = (snd rQ1).createdAt
    }

sre_rQ1Updated' :: U.UUID -> ProjectEvent
sre_rQ1Updated' = SetReplyEvent' . sre_rQ1Updated

sre_rQ1Updated :: U.UUID -> SetReplyEvent
sre_rQ1Updated projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "ede0aa4d6c5a"
    , path = fst rQ1Updated
    , value = (snd rQ1Updated).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ1Updated).createdBy
    , createdAt = (snd rQ1Updated).createdAt
    }

sre_rQ1Dto' :: U.UUID -> ProjectEventDTO
sre_rQ1Dto' projectUuid = toEventDTO (sre_rQ1' projectUuid) (Just userAlbert)

sre_rQ2' :: U.UUID -> ProjectEvent
sre_rQ2' = SetReplyEvent' . sre_rQ2

sre_rQ2 :: U.UUID -> SetReplyEvent
sre_rQ2 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "5dc60233046e"
    , path = fst rQ2
    , value = (snd rQ2).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ2).createdBy
    , createdAt = (snd rQ2).createdAt
    }

sre_rQ2_aYes_fuQ1' :: U.UUID -> ProjectEvent
sre_rQ2_aYes_fuQ1' = SetReplyEvent' . sre_rQ2_aYes_fuQ1

sre_rQ2_aYes_fuQ1 :: U.UUID -> SetReplyEvent
sre_rQ2_aYes_fuQ1 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "c4db3ec5fadd"
    , path = fst rQ2_aYes_fuQ1
    , value = (snd rQ2_aYes_fuQ1).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ2_aYes_fuQ1).createdBy
    , createdAt = (snd rQ2_aYes_fuQ1).createdAt
    }

sre_rQ3' :: U.UUID -> ProjectEvent
sre_rQ3' = SetReplyEvent' . sre_rQ3

sre_rQ3 :: U.UUID -> SetReplyEvent
sre_rQ3 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "cf42760517d7"
    , path = fst rQ3
    , value = (snd rQ3).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ3).createdBy
    , createdAt = (snd rQ3).createdAt
    }

sre_rQ4' :: U.UUID -> ProjectEvent
sre_rQ4' = SetReplyEvent' . sre_rQ4

sre_rQ4 :: U.UUID -> SetReplyEvent
sre_rQ4 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "52d6816a471c"
    , path = fst rQ4
    , value = (snd rQ4).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ4).createdBy
    , createdAt = (snd rQ4).createdAt
    }

sre_rQ4_it1_q5' :: U.UUID -> ProjectEvent
sre_rQ4_it1_q5' = SetReplyEvent' . sre_rQ4_it1_q5

sre_rQ4_it1_q5 :: U.UUID -> SetReplyEvent
sre_rQ4_it1_q5 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "51954a9feb0b"
    , path = fst rQ4_it1_q5
    , value = (snd rQ4_it1_q5).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ4_it1_q5).createdBy
    , createdAt = (snd rQ4_it1_q5).createdAt
    }

sre_rQ4_it1_q5_it1_question7' :: U.UUID -> ProjectEvent
sre_rQ4_it1_q5_it1_question7' = SetReplyEvent' . sre_rQ4_it1_q5_it1_question7

sre_rQ4_it1_q5_it1_question7 :: U.UUID -> SetReplyEvent
sre_rQ4_it1_q5_it1_question7 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "7927b71393bc"
    , path = fst rQ4_it1_q5_it1_question7
    , value = (snd rQ4_it1_q5_it1_question7).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ4_it1_q5_it1_question7).createdBy
    , createdAt = (snd rQ4_it1_q5_it1_question7).createdAt
    }

sre_rQ4_it1_q5_it1_question8' :: U.UUID -> ProjectEvent
sre_rQ4_it1_q5_it1_question8' = SetReplyEvent' . sre_rQ4_it1_q5_it1_question8

sre_rQ4_it1_q5_it1_question8 :: U.UUID -> SetReplyEvent
sre_rQ4_it1_q5_it1_question8 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "a8b3312ce8be"
    , path = fst rQ4_it1_q5_it1_question8
    , value = (snd rQ4_it1_q5_it1_question8).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ4_it1_q5_it1_question8).createdBy
    , createdAt = (snd rQ4_it1_q5_it1_question8).createdAt
    }

sre_rQ4_it1_q6' :: U.UUID -> ProjectEvent
sre_rQ4_it1_q6' = SetReplyEvent' . sre_rQ4_it1_q6

sre_rQ4_it1_q6 :: U.UUID -> SetReplyEvent
sre_rQ4_it1_q6 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "d270a461c2bb"
    , path = fst rQ4_it1_q6
    , value = (snd rQ4_it1_q6).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ4_it1_q6).createdBy
    , createdAt = (snd rQ4_it1_q6).createdAt
    }

sre_rQ4_it2_q5' :: U.UUID -> ProjectEvent
sre_rQ4_it2_q5' = SetReplyEvent' . sre_rQ4_it2_q5

sre_rQ4_it2_q5 :: U.UUID -> SetReplyEvent
sre_rQ4_it2_q5 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "e30b5086cea2"
    , path = fst rQ4_it2_q5
    , value = (snd rQ4_it2_q5).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ4_it2_q5).createdBy
    , createdAt = (snd rQ4_it2_q5).createdAt
    }

sre_rQ4_it2_q6' :: U.UUID -> ProjectEvent
sre_rQ4_it2_q6' = SetReplyEvent' . sre_rQ4_it2_q6

sre_rQ4_it2_q6 :: U.UUID -> SetReplyEvent
sre_rQ4_it2_q6 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "8a83232defb2"
    , path = fst rQ4_it2_q6
    , value = (snd rQ4_it2_q6).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ4_it2_q6).createdBy
    , createdAt = (snd rQ4_it2_q6).createdAt
    }

sre_rQ9' :: U.UUID -> ProjectEvent
sre_rQ9' = SetReplyEvent' . sre_rQ9

sre_rQ9 :: U.UUID -> SetReplyEvent
sre_rQ9 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "064a51dfe89a"
    , path = fst rQ9
    , value = (snd rQ9).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ9).createdBy
    , createdAt = (snd rQ9).createdAt
    }

sre_rQ10' :: U.UUID -> ProjectEvent
sre_rQ10' = SetReplyEvent' . sre_rQ10

sre_rQ10 :: U.UUID -> SetReplyEvent
sre_rQ10 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "40df71a81b92"
    , path = fst rQ10
    , value = (snd rQ10).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ10).createdBy
    , createdAt = (snd rQ10).createdAt
    }

sre_rQ11' :: U.UUID -> ProjectEvent
sre_rQ11' = SetReplyEvent' . sre_rQ11

sre_rQ11 :: U.UUID -> SetReplyEvent
sre_rQ11 projectUuid =
  SetReplyEvent
    { uuid = createEventUuid projectUuid "c4f4481d5670"
    , path = fst rQ11
    , value = (snd rQ11).value
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = fmap (.uuid) $ (snd rQ11).createdBy
    , createdAt = (snd rQ11).createdAt
    }

cre_rQ1' :: U.UUID -> ProjectEvent
cre_rQ1' = ClearReplyEvent' . cre_rQ1

cre_rQ1 :: U.UUID -> ClearReplyEvent
cre_rQ1 projectUuid =
  ClearReplyEvent
    { uuid = createEventUuid projectUuid "c513e14de55e"
    , path = fst rQ1
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 17
    }

sphse_1' :: U.UUID -> ProjectEvent
sphse_1' = SetPhaseEvent' . sphse_1

sphse_1 :: U.UUID -> SetPhaseEvent
sphse_1 projectUuid =
  SetPhaseEvent
    { uuid = createEventUuid projectUuid "ee411f0005d3"
    , phaseUuid = Just $ phase1.uuid
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 18
    }

sphse_2' :: U.UUID -> ProjectEvent
sphse_2' = SetPhaseEvent' . sphse_2

sphse_2 :: U.UUID -> SetPhaseEvent
sphse_2 projectUuid =
  SetPhaseEvent
    { uuid = createEventUuid projectUuid "43eae0986894"
    , phaseUuid = Just $ phase2.uuid
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 19
    }

sphse_3' :: U.UUID -> ProjectEvent
sphse_3' = SetPhaseEvent' . sphse_3

sphse_3 :: U.UUID -> SetPhaseEvent
sphse_3 projectUuid =
  SetPhaseEvent
    { uuid = createEventUuid projectUuid "dacc8f77de05"
    , phaseUuid = Just $ phase3.uuid
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 20
    }

slble_rQ1' :: U.UUID -> ProjectEvent
slble_rQ1' = SetLabelsEvent' . slble_rQ1

slble_rQ1 :: U.UUID -> SetLabelsEvent
slble_rQ1 projectUuid =
  SetLabelsEvent
    { uuid = createEventUuid projectUuid "dd016270ce7e"
    , path = fst rQ1
    , value = [fLabel1]
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 21
    }

slble_rQ2' :: U.UUID -> ProjectEvent
slble_rQ2' = SetLabelsEvent' . slble_rQ2

slble_rQ2 :: U.UUID -> SetLabelsEvent
slble_rQ2 projectUuid =
  SetLabelsEvent
    { uuid = createEventUuid projectUuid "e2acc52bf8db"
    , path = fst rQ2
    , value = [fLabel1]
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 22
    }

rte_rQ1_t1' :: ProjectEventDTO
rte_rQ1_t1' = ResolveCommentThreadEventDTO' rte_rQ1_t1

rte_rQ1_t1 :: ResolveCommentThreadEventDTO
rte_rQ1_t1 =
  ResolveCommentThreadEventDTO
    { uuid = u' "ad5ffe15-d895-4452-af31-3b952db0b8a8"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentCount = 1
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ1_t1Resolved.createdAt
    }

rtche_rQ1_t1' :: ProjectEventChangeDTO
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

ote_rQ1_t1' :: ProjectEventDTO
ote_rQ1_t1' = ReopenCommentThreadEventDTO' ote_rQ1_t1

ote_rQ1_t1 :: ReopenCommentThreadEventDTO
ote_rQ1_t1 =
  ReopenCommentThreadEventDTO
    { uuid = u' "444c89c8-ead9-44c7-9621-0c0c43ff5f9f"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentCount = 1
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ1_t1.createdAt
    }

otche_rQ1_t1' :: ProjectEventChangeDTO
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

aste_rQ1_t1' :: ProjectEventDTO
aste_rQ1_t1' = AssignCommentThreadEventDTO' aste_rQ1_t1

aste_rQ1_t1 :: AssignCommentThreadEventDTO
aste_rQ1_t1 =
  AssignCommentThreadEventDTO
    { uuid = u' "444c89c8-ead9-44c7-9621-0c0c43ff5f9f"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , private = False
    , assignedTo = Just . toSuggestion . toSimple $ userAlbert
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ1_t1.createdAt
    }

asche_rQ1_t1' :: ProjectEventChangeDTO
asche_rQ1_t1' = AssignCommentThreadEventChangeDTO' asche_rQ1_t1

asche_rQ1_t1 :: AssignCommentThreadEventChangeDTO
asche_rQ1_t1 =
  AssignCommentThreadEventChangeDTO
    { uuid = aste_rQ1_t1.uuid
    , path = aste_rQ1_t1.path
    , threadUuid = aste_rQ1_t1.threadUuid
    , private = aste_rQ1_t1.private
    , assignedTo = aste_rQ1_t1.assignedTo
    }

dte_rQ1_t1' :: ProjectEventDTO
dte_rQ1_t1' = DeleteCommentThreadEventDTO' dte_rQ1_t1

dte_rQ1_t1 :: DeleteCommentThreadEventDTO
dte_rQ1_t1 =
  DeleteCommentThreadEventDTO
    { uuid = u' "0e8a5812-90da-43b1-bb20-dbf8a95aa00d"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ1_t1.createdAt
    }

dtche_rQ1_t1' :: ProjectEventChangeDTO
dtche_rQ1_t1' = DeleteCommentThreadEventChangeDTO' dtche_rQ1_t1

dtche_rQ1_t1 :: DeleteCommentThreadEventChangeDTO
dtche_rQ1_t1 =
  DeleteCommentThreadEventChangeDTO
    { uuid = dte_rQ1_t1.uuid
    , path = dte_rQ1_t1.path
    , threadUuid = dte_rQ1_t1.threadUuid
    , private = False
    }

ace_rQ1_t1_1' :: ProjectEventDTO
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
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ1_t1_1.createdAt
    }

ace_rQ1_t1_2' :: ProjectEventDTO
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
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ1_t1_2.createdAt
    }

ace_rQ2_t1_1' :: ProjectEventDTO
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
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ2_t1_1.createdAt
    }

acche_rQ2_t1_1' :: ProjectEventChangeDTO
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

ece_rQ1_t1_1' :: ProjectEventDTO
ece_rQ1_t1_1' = EditCommentEventDTO' ece_rQ1_t1_1

ece_rQ1_t1_1 :: EditCommentEventDTO
ece_rQ1_t1_1 =
  EditCommentEventDTO
    { uuid = u' "6a598663-4bce-48e9-83d0-422ae753f60d"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentUuid = cmtQ1_t1_1Edited.uuid
    , text = cmtQ1_t1_1Edited.text
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ1_t1_1Edited.updatedAt
    }

ecche_rQ1_t1_1' :: ProjectEventChangeDTO
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

dce_rQ1_t1_1' :: ProjectEventDTO
dce_rQ1_t1_1' = DeleteCommentEventDTO' dce_rQ1_t1_1

dce_rQ1_t1_1 :: DeleteCommentEventDTO
dce_rQ1_t1_1 =
  DeleteCommentEventDTO
    { uuid = u' "0e8a5812-90da-43b1-bb20-dbf8a95aa00d"
    , path = cmtQ1_path
    , threadUuid = cmtQ1_t1.uuid
    , commentUuid = cmtQ1_t1_1.uuid
    , createdBy = Just . toSuggestion . toSimple $ userAlbert
    , createdAt = cmtQ1_t1_1.createdAt
    }

dcche_rQ1_t1_1' :: ProjectEventChangeDTO
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

createEventUuid :: U.UUID -> String -> U.UUID
createEventUuid projectUuid eventSuffix =
  let parts = splitOn "-" . U.toString $ projectUuid
   in u' . L.intercalate "-" $ [head parts, parts !! 1, parts !! 2, parts !! 3, eventSuffix]
