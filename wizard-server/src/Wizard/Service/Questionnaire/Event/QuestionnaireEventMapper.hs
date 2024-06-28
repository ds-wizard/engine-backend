module Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

toEventDTO :: QuestionnaireEvent -> Maybe User -> QuestionnaireEventDTO
toEventDTO event' mCreatedBy =
  case event' of
    SetReplyEvent' event@SetReplyEvent {..} -> SetReplyEventDTO' $ toSetReplyEventDTO event mCreatedBy
    ClearReplyEvent' event@ClearReplyEvent {..} -> ClearReplyEventDTO' $ toClearReplyEventDTO event mCreatedBy
    SetPhaseEvent' event@SetPhaseEvent {..} -> SetPhaseEventDTO' $ toSetPhaseEventDTO event mCreatedBy
    SetLabelsEvent' event@SetLabelsEvent {..} -> SetLabelsEventDTO' $ toSetLabelsEventDTO event mCreatedBy

toSetReplyEventDTO :: SetReplyEvent -> Maybe User -> SetReplyEventDTO
toSetReplyEventDTO event user =
  SetReplyEventDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , createdAt = event.createdAt
    }

toClearReplyEventDTO :: ClearReplyEvent -> Maybe User -> ClearReplyEventDTO
toClearReplyEventDTO event user =
  ClearReplyEventDTO
    { uuid = event.uuid
    , path = event.path
    , createdBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , createdAt = event.createdAt
    }

toSetPhaseEventDTO :: SetPhaseEvent -> Maybe User -> SetPhaseEventDTO
toSetPhaseEventDTO event user =
  SetPhaseEventDTO
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    , createdBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , createdAt = event.createdAt
    }

toSetLabelsEventDTO :: SetLabelsEvent -> Maybe User -> SetLabelsEventDTO
toSetLabelsEventDTO event user =
  SetLabelsEventDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , createdAt = event.createdAt
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toSetReplyEventDTO' :: SetReplyEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> SetReplyEventDTO
toSetReplyEventDTO' event mCreatedBy now =
  SetReplyEventDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = mCreatedBy
    , createdAt = now
    }

toClearReplyEventDTO' :: ClearReplyEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> ClearReplyEventDTO
toClearReplyEventDTO' event mCreatedBy now =
  ClearReplyEventDTO
    { uuid = event.uuid
    , path = event.path
    , createdBy = mCreatedBy
    , createdAt = now
    }

toSetPhaseEventDTO' :: SetPhaseEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> SetPhaseEventDTO
toSetPhaseEventDTO' event mCreatedBy now =
  SetPhaseEventDTO
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    , createdBy = mCreatedBy
    , createdAt = now
    }

toSetLabelsEventDTO' :: SetLabelsEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> SetLabelsEventDTO
toSetLabelsEventDTO' event mCreatedBy now =
  SetLabelsEventDTO
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = mCreatedBy
    , createdAt = now
    }

toResolveCommentThreadEventDTO'
  :: ResolveCommentThreadEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> ResolveCommentThreadEventDTO
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
  :: ReopenCommentThreadEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> ReopenCommentThreadEventDTO
toReopenCommentThreadEventDTO' event mCreatedBy now =
  ReopenCommentThreadEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , commentCount = event.commentCount
    , createdBy = mCreatedBy
    , createdAt = now
    }

toAssignCommentThreadEventDTO' :: AssignCommentThreadEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> AssignCommentThreadEventDTO
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
  :: DeleteCommentThreadEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> DeleteCommentThreadEventDTO
toDeleteCommentThreadEventDTO' event mCreatedBy now =
  DeleteCommentThreadEventDTO
    { uuid = event.uuid
    , path = event.path
    , threadUuid = event.threadUuid
    , createdBy = mCreatedBy
    , createdAt = now
    }

toAddCommentEventDTO' :: AddCommentEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> AddCommentEventDTO
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

toEditCommentEventDTO' :: EditCommentEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> EditCommentEventDTO
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

toDeleteCommentEventDTO' :: DeleteCommentEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> DeleteCommentEventDTO
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
toEventChangeDTO :: QuestionnaireEvent -> QuestionnaireEventChangeDTO
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
fromEventChangeDTO :: QuestionnaireEventChangeDTO -> Maybe U.UUID -> UTCTime -> QuestionnaireEvent
fromEventChangeDTO event createdBy now =
  case event of
    SetReplyEventChangeDTO' event@SetReplyEventChangeDTO {..} ->
      SetReplyEvent' $ fromSetReplyEventChangeDTO event createdBy now
    ClearReplyEventChangeDTO' event@ClearReplyEventChangeDTO {..} ->
      ClearReplyEvent' $ fromClearReplyEventChangeDTO event createdBy now
    SetPhaseEventChangeDTO' event@SetPhaseEventChangeDTO {..} ->
      SetPhaseEvent' $ fromSetPhaseEventChangeDTO event createdBy now
    SetLabelsEventChangeDTO' event@SetLabelsEventChangeDTO {..} ->
      SetLabelsEvent' $ fromSetLabelsEventChangeDTO event createdBy now

fromSetReplyEventChangeDTO :: SetReplyEventChangeDTO -> Maybe U.UUID -> UTCTime -> SetReplyEvent
fromSetReplyEventChangeDTO event createdBy now =
  SetReplyEvent
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = createdBy
    , createdAt = now
    }

fromClearReplyEventChangeDTO :: ClearReplyEventChangeDTO -> Maybe U.UUID -> UTCTime -> ClearReplyEvent
fromClearReplyEventChangeDTO event createdBy now =
  ClearReplyEvent
    { uuid = event.uuid
    , path = event.path
    , createdBy = createdBy
    , createdAt = now
    }

fromSetPhaseEventChangeDTO :: SetPhaseEventChangeDTO -> Maybe U.UUID -> UTCTime -> SetPhaseEvent
fromSetPhaseEventChangeDTO event createdBy now =
  SetPhaseEvent
    { uuid = event.uuid
    , phaseUuid = event.phaseUuid
    , createdBy = createdBy
    , createdAt = now
    }

fromSetLabelsEventChangeDTO :: SetLabelsEventChangeDTO -> Maybe U.UUID -> UTCTime -> SetLabelsEvent
fromSetLabelsEventChangeDTO event createdBy now =
  SetLabelsEvent
    { uuid = event.uuid
    , path = event.path
    , value = event.value
    , createdBy = createdBy
    , createdAt = now
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toReply :: SetReplyEvent -> Maybe User -> Reply
toReply event mUser =
  Reply
    { value = event.value
    , createdBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , createdAt = event.createdAt
    }
