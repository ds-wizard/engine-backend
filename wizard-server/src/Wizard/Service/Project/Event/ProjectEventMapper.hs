module Wizard.Service.Project.Event.ProjectEventMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Api.Resource.Project.Event.ProjectEventDTO
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.ProjectReply
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM
import WizardLib.Public.Model.User.UserSuggestion

toEventDTO :: ProjectEvent -> Maybe User -> ProjectEventDTO
toEventDTO event' mCreatedBy =
  case event' of
    SetReplyEvent' event@SetReplyEvent {..} -> SetReplyEventDTO' $ toSetReplyEventDTO event mCreatedBy
    ClearReplyEvent' event@ClearReplyEvent {..} -> ClearReplyEventDTO' $ toClearReplyEventDTO event mCreatedBy
    SetPhaseEvent' event@SetPhaseEvent {..} -> SetPhaseEventDTO' $ toSetPhaseEventDTO event mCreatedBy
    SetLabelsEvent' event@SetLabelsEvent {..} -> SetLabelsEventDTO' $ toSetLabelsEventDTO event mCreatedBy

toEventList :: ProjectEvent -> Maybe User -> ProjectEventList
toEventList event' mCreatedBy =
  case event' of
    SetReplyEvent' event@SetReplyEvent {..} -> SetReplyEventList' $ toSetReplyEventList event mCreatedBy
    ClearReplyEvent' event@ClearReplyEvent {..} -> ClearReplyEventList' $ toClearReplyEventList event mCreatedBy
    SetPhaseEvent' event@SetPhaseEvent {..} -> SetPhaseEventList' $ toSetPhaseEventList event mCreatedBy
    SetLabelsEvent' event@SetLabelsEvent {..} -> SetLabelsEventList' $ toSetLabelsEventList event mCreatedBy

toEvent :: U.UUID -> U.UUID -> ProjectEventList -> ProjectEvent
toEvent projectUuid tenantUuid event' =
  case event' of
    SetReplyEventList' event@SetReplyEventList {..} -> SetReplyEvent' $ toSetReplyEvent projectUuid tenantUuid event
    ClearReplyEventList' event@ClearReplyEventList {..} -> ClearReplyEvent' $ toClearReplyEvent projectUuid tenantUuid event
    SetPhaseEventList' event@SetPhaseEventList {..} -> SetPhaseEvent' $ toSetPhaseEvent projectUuid tenantUuid event
    SetLabelsEventList' event@SetLabelsEventList {..} -> SetLabelsEvent' $ toSetLabelsEvent projectUuid tenantUuid event

toSetReplyEventDTO :: SetReplyEvent -> Maybe User -> SetReplyEventDTO
toSetReplyEventDTO event user =
  SetReplyEventDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) user
    , createdAt = event.createdAt
    }

toSetReplyEventList :: SetReplyEvent -> Maybe User -> SetReplyEventList
toSetReplyEventList event user =
  SetReplyEventList
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) user
    , createdAt = event.createdAt
    }

toSetReplyEvent :: U.UUID -> U.UUID -> SetReplyEventList -> SetReplyEvent
toSetReplyEvent projectUuid tenantUuid event =
  SetReplyEvent
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = fmap (.uuid) event.createdBy
    , createdAt = event.createdAt
    }

toClearReplyEventDTO :: ClearReplyEvent -> Maybe User -> ClearReplyEventDTO
toClearReplyEventDTO event user =
  ClearReplyEventDTO
    { uuid = event.uuid
    , path = event.path
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) user
    , createdAt = event.createdAt
    }

toClearReplyEventList :: ClearReplyEvent -> Maybe User -> ClearReplyEventList
toClearReplyEventList event user =
  ClearReplyEventList
    { uuid = event.uuid
    , path = event.path
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) user
    , createdAt = event.createdAt
    }

toClearReplyEvent :: U.UUID -> U.UUID -> ClearReplyEventList -> ClearReplyEvent
toClearReplyEvent projectUuid tenantUuid event =
  ClearReplyEvent
    { uuid = event.uuid
    , path = event.path
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = fmap (.uuid) event.createdBy
    , createdAt = event.createdAt
    }

toSetPhaseEventDTO :: SetPhaseEvent -> Maybe User -> SetPhaseEventDTO
toSetPhaseEventDTO event user =
  SetPhaseEventDTO
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) user
    , createdAt = event.createdAt
    }

toSetPhaseEventList :: SetPhaseEvent -> Maybe User -> SetPhaseEventList
toSetPhaseEventList event user =
  SetPhaseEventList
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) user
    , createdAt = event.createdAt
    }

toSetPhaseEvent :: U.UUID -> U.UUID -> SetPhaseEventList -> SetPhaseEvent
toSetPhaseEvent projectUuid tenantUuid event =
  SetPhaseEvent
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = fmap (.uuid) event.createdBy
    , createdAt = event.createdAt
    }

toSetLabelsEventDTO :: SetLabelsEvent -> Maybe User -> SetLabelsEventDTO
toSetLabelsEventDTO event user =
  SetLabelsEventDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) user
    , createdAt = event.createdAt
    }

toSetLabelsEventList :: SetLabelsEvent -> Maybe User -> SetLabelsEventList
toSetLabelsEventList event user =
  SetLabelsEventList
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) user
    , createdAt = event.createdAt
    }

toSetLabelsEvent :: U.UUID -> U.UUID -> SetLabelsEventList -> SetLabelsEvent
toSetLabelsEvent projectUuid tenantUuid event =
  SetLabelsEvent
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = fmap (.uuid) event.createdBy
    , createdAt = event.createdAt
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toSetReplyEventDTO' :: SetReplyEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> SetReplyEventDTO
toSetReplyEventDTO' event mCreatedBy now =
  SetReplyEventDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = mCreatedBy
    , createdAt = now
    }

toClearReplyEventDTO' :: ClearReplyEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> ClearReplyEventDTO
toClearReplyEventDTO' event mCreatedBy now =
  ClearReplyEventDTO
    { uuid = event.uuid
    , path = event.path
    , createdBy = mCreatedBy
    , createdAt = now
    }

toSetPhaseEventDTO' :: SetPhaseEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> SetPhaseEventDTO
toSetPhaseEventDTO' event mCreatedBy now =
  SetPhaseEventDTO
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    , createdBy = mCreatedBy
    , createdAt = now
    }

toSetLabelsEventDTO' :: SetLabelsEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> SetLabelsEventDTO
toSetLabelsEventDTO' event mCreatedBy now =
  SetLabelsEventDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = mCreatedBy
    , createdAt = now
    }

toResolveCommentThreadEventDTO'
  :: ResolveCommentThreadEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> ResolveCommentThreadEventDTO
toResolveCommentThreadEventDTO' event mCreatedBy now =
  ResolveCommentThreadEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , commentCount = event.commentCount
    , createdBy = mCreatedBy
    , createdAt = now
    }

toReopenCommentThreadEventDTO'
  :: ReopenCommentThreadEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> ReopenCommentThreadEventDTO
toReopenCommentThreadEventDTO' event mCreatedBy now =
  ReopenCommentThreadEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , commentCount = event.commentCount
    , createdBy = mCreatedBy
    , createdAt = now
    }

toAssignCommentThreadEventDTO' :: AssignCommentThreadEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> AssignCommentThreadEventDTO
toAssignCommentThreadEventDTO' event mCreatedBy now =
  AssignCommentThreadEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , private = event.private
    , assignedTo = event.assignedTo
    , createdBy = mCreatedBy
    , createdAt = now
    }

toDeleteCommentThreadEventDTO'
  :: DeleteCommentThreadEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> DeleteCommentThreadEventDTO
toDeleteCommentThreadEventDTO' event mCreatedBy now =
  DeleteCommentThreadEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , createdBy = mCreatedBy
    , createdAt = now
    }

toAddCommentEventDTO' :: AddCommentEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> AddCommentEventDTO
toAddCommentEventDTO' event mCreatedBy now =
  AddCommentEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , commentUuid = event.commentUuid
    , text = event.text
    , private = event.private
    , createdBy = mCreatedBy
    , createdAt = now
    }

toEditCommentEventDTO' :: EditCommentEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> EditCommentEventDTO
toEditCommentEventDTO' event mCreatedBy now =
  EditCommentEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , commentUuid = event.commentUuid
    , text = event.text
    , createdBy = mCreatedBy
    , createdAt = now
    }

toDeleteCommentEventDTO' :: DeleteCommentEventChangeDTO -> Maybe UserSuggestion -> UTCTime -> DeleteCommentEventDTO
toDeleteCommentEventDTO' event mCreatedBy now =
  DeleteCommentEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , commentUuid = event.commentUuid
    , createdBy = mCreatedBy
    , createdAt = now
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toEventChangeDTO :: ProjectEvent -> ProjectEventChangeDTO
toEventChangeDTO event =
  case event of
    SetReplyEvent' event@SetReplyEvent {..} ->
      SetReplyEventChangeDTO' $ toSetReplyEventChangeDTO event
    ClearReplyEvent' event@ClearReplyEvent {..} ->
      ClearReplyEventChangeDTO' $ toClearReplyEventChangeDTO event
    SetPhaseEvent' event@SetPhaseEvent {..} ->
      SetPhaseEventChangeDTO' $ toSetPhaseEventChangeDTO event
    SetLabelsEvent' event@SetLabelsEvent {..} ->
      SetLabelsEventChangeDTO' $ toSetLabelsEventChangeDTO event

toSetReplyEventChangeDTO :: SetReplyEvent -> SetReplyEventChangeDTO
toSetReplyEventChangeDTO event =
  SetReplyEventChangeDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    }

toClearReplyEventChangeDTO :: ClearReplyEvent -> ClearReplyEventChangeDTO
toClearReplyEventChangeDTO event =
  ClearReplyEventChangeDTO
    { uuid = event.uuid
    , path = event.path
    }

toSetPhaseEventChangeDTO :: SetPhaseEvent -> SetPhaseEventChangeDTO
toSetPhaseEventChangeDTO event =
  SetPhaseEventChangeDTO
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    }

toSetLabelsEventChangeDTO :: SetLabelsEvent -> SetLabelsEventChangeDTO
toSetLabelsEventChangeDTO event =
  SetLabelsEventChangeDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
fromEventChangeDTO :: ProjectEventChangeDTO -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> ProjectEvent
fromEventChangeDTO event projectUuid tenantUuid createdBy now =
  case event of
    SetReplyEventChangeDTO' event@SetReplyEventChangeDTO {..} ->
      SetReplyEvent' $ fromSetReplyEventChangeDTO event projectUuid tenantUuid createdBy now
    ClearReplyEventChangeDTO' event@ClearReplyEventChangeDTO {..} ->
      ClearReplyEvent' $ fromClearReplyEventChangeDTO event projectUuid tenantUuid createdBy now
    SetPhaseEventChangeDTO' event@SetPhaseEventChangeDTO {..} ->
      SetPhaseEvent' $ fromSetPhaseEventChangeDTO event projectUuid tenantUuid createdBy now
    SetLabelsEventChangeDTO' event@SetLabelsEventChangeDTO {..} ->
      SetLabelsEvent' $ fromSetLabelsEventChangeDTO event projectUuid tenantUuid createdBy now
    _ -> error "Unsupported event type in fromEventChangeDTO"

fromSetReplyEventChangeDTO :: SetReplyEventChangeDTO -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> SetReplyEvent
fromSetReplyEventChangeDTO event projectUuid tenantUuid createdBy now =
  SetReplyEvent
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = createdBy
    , createdAt = now
    }

fromClearReplyEventChangeDTO :: ClearReplyEventChangeDTO -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> ClearReplyEvent
fromClearReplyEventChangeDTO event projectUuid tenantUuid createdBy now =
  ClearReplyEvent
    { uuid = event.uuid
    , path = event.path
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = createdBy
    , createdAt = now
    }

fromSetPhaseEventChangeDTO :: SetPhaseEventChangeDTO -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> SetPhaseEvent
fromSetPhaseEventChangeDTO event projectUuid tenantUuid createdBy now =
  SetPhaseEvent
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = createdBy
    , createdAt = now
    }

fromSetLabelsEventChangeDTO :: SetLabelsEventChangeDTO -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> SetLabelsEvent
fromSetLabelsEventChangeDTO event projectUuid tenantUuid createdBy now =
  SetLabelsEvent
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = createdBy
    , createdAt = now
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toReply :: SetReplyEvent -> Maybe User -> Reply
toReply event mUser =
  Reply
    { value = event.value
    , createdBy = fmap (UM.toSuggestion . UM.toSimple) mUser
    , createdAt = event.createdAt
    }

toReply' :: SetReplyEventList -> Reply
toReply' SetReplyEventList {..} = Reply {..}
