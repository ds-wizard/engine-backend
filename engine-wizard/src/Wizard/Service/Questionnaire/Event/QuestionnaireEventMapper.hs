module Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper where

import Control.Lens ((^.), (^?), _Just)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM

toEventDTO :: QuestionnaireEvent -> Maybe User -> QuestionnaireEventDTO
toEventDTO event' mCreatedBy =
  case event' of
    SetReplyEvent' event@SetReplyEvent {..} -> SetReplyEventDTO' $ toSetReplyEventDTO event mCreatedBy
    ClearReplyEvent' event@ClearReplyEvent {..} -> ClearReplyEventDTO' $ toClearReplyEventDTO event mCreatedBy
    SetPhaseEvent' event@SetPhaseEvent {..} -> SetPhaseEventDTO' $ toSetPhaseEventDTO event mCreatedBy
    SetLabelsEvent' event@SetLabelsEvent {..} -> SetLabelsEventDTO' $ toSetLabelsEventDTO event mCreatedBy
    ResolveCommentThreadEvent' event@ResolveCommentThreadEvent {..} ->
      ResolveCommentThreadEventDTO' $ toResolveCommentThreadEventDTO event mCreatedBy
    ReopenCommentThreadEvent' event@ReopenCommentThreadEvent {..} ->
      ReopenCommentThreadEventDTO' $ toReopenCommentThreadEventDTO event mCreatedBy
    DeleteCommentThreadEvent' event@DeleteCommentThreadEvent {..} ->
      DeleteCommentThreadEventDTO' $ toDeleteCommentThreadEventDTO event mCreatedBy
    AddCommentEvent' event@AddCommentEvent {..} -> AddCommentEventDTO' $ toAddCommentEventDTO event mCreatedBy
    EditCommentEvent' event@EditCommentEvent {..} -> EditCommentEventDTO' $ toEditCommentEventDTO event mCreatedBy
    DeleteCommentEvent' event@DeleteCommentEvent {..} ->
      DeleteCommentEventDTO' $ toDeleteCommentEventDTO event mCreatedBy

toSetReplyEventDTO :: SetReplyEvent -> Maybe User -> SetReplyEventDTO
toSetReplyEventDTO event user =
  SetReplyEventDTO
    { _setReplyEventDTOUuid = event ^. uuid
    , _setReplyEventDTOPath = event ^. path
    , _setReplyEventDTOValue = event ^. value
    , _setReplyEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _setReplyEventDTOCreatedAt = event ^. createdAt
    }

toClearReplyEventDTO :: ClearReplyEvent -> Maybe User -> ClearReplyEventDTO
toClearReplyEventDTO event user =
  ClearReplyEventDTO
    { _clearReplyEventDTOUuid = event ^. uuid
    , _clearReplyEventDTOPath = event ^. path
    , _clearReplyEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _clearReplyEventDTOCreatedAt = event ^. createdAt
    }

toSetPhaseEventDTO :: SetPhaseEvent -> Maybe User -> SetPhaseEventDTO
toSetPhaseEventDTO event user =
  SetPhaseEventDTO
    { _setPhaseEventDTOUuid = event ^. uuid
    , _setPhaseEventDTOPhaseUuid = event ^. phaseUuid
    , _setPhaseEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _setPhaseEventDTOCreatedAt = event ^. createdAt
    }

toSetLabelsEventDTO :: SetLabelsEvent -> Maybe User -> SetLabelsEventDTO
toSetLabelsEventDTO event user =
  SetLabelsEventDTO
    { _setLabelsEventDTOUuid = event ^. uuid
    , _setLabelsEventDTOPath = event ^. path
    , _setLabelsEventDTOValue = event ^. value
    , _setLabelsEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _setLabelsEventDTOCreatedAt = event ^. createdAt
    }

toResolveCommentThreadEventDTO :: ResolveCommentThreadEvent -> Maybe User -> ResolveCommentThreadEventDTO
toResolveCommentThreadEventDTO event user =
  ResolveCommentThreadEventDTO
    { _resolveCommentThreadEventDTOUuid = event ^. uuid
    , _resolveCommentThreadEventDTOPath = event ^. path
    , _resolveCommentThreadEventDTOThreadUuid = event ^. threadUuid
    , _resolveCommentThreadEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _resolveCommentThreadEventDTOCreatedAt = event ^. createdAt
    }

toReopenCommentThreadEventDTO :: ReopenCommentThreadEvent -> Maybe User -> ReopenCommentThreadEventDTO
toReopenCommentThreadEventDTO event user =
  ReopenCommentThreadEventDTO
    { _reopenCommentThreadEventDTOUuid = event ^. uuid
    , _reopenCommentThreadEventDTOPath = event ^. path
    , _reopenCommentThreadEventDTOThreadUuid = event ^. threadUuid
    , _reopenCommentThreadEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _reopenCommentThreadEventDTOCreatedAt = event ^. createdAt
    }

toDeleteCommentThreadEventDTO :: DeleteCommentThreadEvent -> Maybe User -> DeleteCommentThreadEventDTO
toDeleteCommentThreadEventDTO event user =
  DeleteCommentThreadEventDTO
    { _deleteCommentThreadEventDTOUuid = event ^. uuid
    , _deleteCommentThreadEventDTOPath = event ^. path
    , _deleteCommentThreadEventDTOThreadUuid = event ^. threadUuid
    , _deleteCommentThreadEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _deleteCommentThreadEventDTOCreatedAt = event ^. createdAt
    }

toAddCommentEventDTO :: AddCommentEvent -> Maybe User -> AddCommentEventDTO
toAddCommentEventDTO event user =
  AddCommentEventDTO
    { _addCommentEventDTOUuid = event ^. uuid
    , _addCommentEventDTOPath = event ^. path
    , _addCommentEventDTOThreadUuid = event ^. threadUuid
    , _addCommentEventDTOCommentUuid = event ^. commentUuid
    , _addCommentEventDTOText = event ^. text
    , _addCommentEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _addCommentEventDTOCreatedAt = event ^. createdAt
    }

toEditCommentEventDTO :: EditCommentEvent -> Maybe User -> EditCommentEventDTO
toEditCommentEventDTO event user =
  EditCommentEventDTO
    { _editCommentEventDTOUuid = event ^. uuid
    , _editCommentEventDTOPath = event ^. path
    , _editCommentEventDTOThreadUuid = event ^. threadUuid
    , _editCommentEventDTOCommentUuid = event ^. commentUuid
    , _editCommentEventDTOText = event ^. text
    , _editCommentEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _editCommentEventDTOCreatedAt = event ^. createdAt
    }

toDeleteCommentEventDTO :: DeleteCommentEvent -> Maybe User -> DeleteCommentEventDTO
toDeleteCommentEventDTO event user =
  DeleteCommentEventDTO
    { _deleteCommentEventDTOUuid = event ^. uuid
    , _deleteCommentEventDTOPath = event ^. path
    , _deleteCommentEventDTOThreadUuid = event ^. threadUuid
    , _deleteCommentEventDTOCommentUuid = event ^. commentUuid
    , _deleteCommentEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _deleteCommentEventDTOCreatedAt = event ^. createdAt
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toEventDTO' :: QuestionnaireEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> QuestionnaireEventDTO
toEventDTO' event' mCreatedBy now =
  case event' of
    SetReplyEventChangeDTO' event@SetReplyEventChangeDTO {..} ->
      SetReplyEventDTO' $ toSetReplyEventDTO' event mCreatedBy now
    ClearReplyEventChangeDTO' event@ClearReplyEventChangeDTO {..} ->
      ClearReplyEventDTO' $ toClearReplyEventDTO' event mCreatedBy now
    SetPhaseEventChangeDTO' event@SetPhaseEventChangeDTO {..} ->
      SetPhaseEventDTO' $ toSetPhaseEventDTO' event mCreatedBy now
    SetLabelsEventChangeDTO' event@SetLabelsEventChangeDTO {..} ->
      SetLabelsEventDTO' $ toSetLabelsEventDTO' event mCreatedBy now
    ResolveCommentThreadEventChangeDTO' event@ResolveCommentThreadEventChangeDTO {..} ->
      ResolveCommentThreadEventDTO' $ toResolveCommentThreadEventDTO' event mCreatedBy now
    ReopenCommentThreadEventChangeDTO' event@ReopenCommentThreadEventChangeDTO {..} ->
      ReopenCommentThreadEventDTO' $ toReopenCommentThreadEventDTO' event mCreatedBy now
    DeleteCommentThreadEventChangeDTO' event@DeleteCommentThreadEventChangeDTO {..} ->
      DeleteCommentThreadEventDTO' $ toDeleteCommentThreadEventDTO' event mCreatedBy now
    AddCommentEventChangeDTO' event@AddCommentEventChangeDTO {..} ->
      AddCommentEventDTO' $ toAddCommentEventDTO' event mCreatedBy now
    EditCommentEventChangeDTO' event@EditCommentEventChangeDTO {..} ->
      EditCommentEventDTO' $ toEditCommentEventDTO' event mCreatedBy now
    DeleteCommentEventChangeDTO' event@DeleteCommentEventChangeDTO {..} ->
      DeleteCommentEventDTO' $ toDeleteCommentEventDTO' event mCreatedBy now

toSetReplyEventDTO' :: SetReplyEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> SetReplyEventDTO
toSetReplyEventDTO' event mCreatedBy now =
  SetReplyEventDTO
    { _setReplyEventDTOUuid = event ^. uuid
    , _setReplyEventDTOPath = event ^. path
    , _setReplyEventDTOValue = event ^. value
    , _setReplyEventDTOCreatedBy = mCreatedBy
    , _setReplyEventDTOCreatedAt = now
    }

toClearReplyEventDTO' :: ClearReplyEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> ClearReplyEventDTO
toClearReplyEventDTO' event mCreatedBy now =
  ClearReplyEventDTO
    { _clearReplyEventDTOUuid = event ^. uuid
    , _clearReplyEventDTOPath = event ^. path
    , _clearReplyEventDTOCreatedBy = mCreatedBy
    , _clearReplyEventDTOCreatedAt = now
    }

toSetPhaseEventDTO' :: SetPhaseEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> SetPhaseEventDTO
toSetPhaseEventDTO' event mCreatedBy now =
  SetPhaseEventDTO
    { _setPhaseEventDTOUuid = event ^. uuid
    , _setPhaseEventDTOPhaseUuid = event ^. phaseUuid
    , _setPhaseEventDTOCreatedBy = mCreatedBy
    , _setPhaseEventDTOCreatedAt = now
    }

toSetLabelsEventDTO' :: SetLabelsEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> SetLabelsEventDTO
toSetLabelsEventDTO' event mCreatedBy now =
  SetLabelsEventDTO
    { _setLabelsEventDTOUuid = event ^. uuid
    , _setLabelsEventDTOPath = event ^. path
    , _setLabelsEventDTOValue = event ^. value
    , _setLabelsEventDTOCreatedBy = mCreatedBy
    , _setLabelsEventDTOCreatedAt = now
    }

toResolveCommentThreadEventDTO' ::
     ResolveCommentThreadEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> ResolveCommentThreadEventDTO
toResolveCommentThreadEventDTO' event mCreatedBy now =
  ResolveCommentThreadEventDTO
    { _resolveCommentThreadEventDTOUuid = event ^. uuid
    , _resolveCommentThreadEventDTOPath = event ^. path
    , _resolveCommentThreadEventDTOThreadUuid = event ^. threadUuid
    , _resolveCommentThreadEventDTOCreatedBy = mCreatedBy
    , _resolveCommentThreadEventDTOCreatedAt = now
    }

toReopenCommentThreadEventDTO' ::
     ReopenCommentThreadEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> ReopenCommentThreadEventDTO
toReopenCommentThreadEventDTO' event mCreatedBy now =
  ReopenCommentThreadEventDTO
    { _reopenCommentThreadEventDTOUuid = event ^. uuid
    , _reopenCommentThreadEventDTOPath = event ^. path
    , _reopenCommentThreadEventDTOThreadUuid = event ^. threadUuid
    , _reopenCommentThreadEventDTOCreatedBy = mCreatedBy
    , _reopenCommentThreadEventDTOCreatedAt = now
    }

toDeleteCommentThreadEventDTO' ::
     DeleteCommentThreadEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> DeleteCommentThreadEventDTO
toDeleteCommentThreadEventDTO' event mCreatedBy now =
  DeleteCommentThreadEventDTO
    { _deleteCommentThreadEventDTOUuid = event ^. uuid
    , _deleteCommentThreadEventDTOPath = event ^. path
    , _deleteCommentThreadEventDTOThreadUuid = event ^. threadUuid
    , _deleteCommentThreadEventDTOCreatedBy = mCreatedBy
    , _deleteCommentThreadEventDTOCreatedAt = now
    }

toAddCommentEventDTO' :: AddCommentEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> AddCommentEventDTO
toAddCommentEventDTO' event mCreatedBy now =
  AddCommentEventDTO
    { _addCommentEventDTOUuid = event ^. uuid
    , _addCommentEventDTOPath = event ^. path
    , _addCommentEventDTOThreadUuid = event ^. threadUuid
    , _addCommentEventDTOCommentUuid = event ^. commentUuid
    , _addCommentEventDTOText = event ^. text
    , _addCommentEventDTOCreatedBy = mCreatedBy
    , _addCommentEventDTOCreatedAt = now
    }

toEditCommentEventDTO' :: EditCommentEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> EditCommentEventDTO
toEditCommentEventDTO' event mCreatedBy now =
  EditCommentEventDTO
    { _editCommentEventDTOUuid = event ^. uuid
    , _editCommentEventDTOPath = event ^. path
    , _editCommentEventDTOThreadUuid = event ^. threadUuid
    , _editCommentEventDTOCommentUuid = event ^. commentUuid
    , _editCommentEventDTOText = event ^. text
    , _editCommentEventDTOCreatedBy = mCreatedBy
    , _editCommentEventDTOCreatedAt = now
    }

toDeleteCommentEventDTO' :: DeleteCommentEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> DeleteCommentEventDTO
toDeleteCommentEventDTO' event mCreatedBy now =
  DeleteCommentEventDTO
    { _deleteCommentEventDTOUuid = event ^. uuid
    , _deleteCommentEventDTOPath = event ^. path
    , _deleteCommentEventDTOThreadUuid = event ^. threadUuid
    , _deleteCommentEventDTOCommentUuid = event ^. commentUuid
    , _deleteCommentEventDTOCreatedBy = mCreatedBy
    , _deleteCommentEventDTOCreatedAt = now
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toEventChangeDTO :: QuestionnaireEvent -> QuestionnaireEventChangeDTO
toEventChangeDTO event =
  case event of
    SetReplyEvent' event@SetReplyEvent {..} -> SetReplyEventChangeDTO' $ toSetReplyEventChangeDTO event
    ClearReplyEvent' event@ClearReplyEvent {..} -> ClearReplyEventChangeDTO' $ toClearReplyEventChangeDTO event
    SetPhaseEvent' event@SetPhaseEvent {..} -> SetPhaseEventChangeDTO' $ toSetPhaseEventChangeDTO event
    SetLabelsEvent' event@SetLabelsEvent {..} -> SetLabelsEventChangeDTO' $ toSetLabelsEventChangeDTO event
    ResolveCommentThreadEvent' event@ResolveCommentThreadEvent {..} ->
      ResolveCommentThreadEventChangeDTO' $ toResolveCommentThreadEventChangeDTO event
    ReopenCommentThreadEvent' event@ReopenCommentThreadEvent {..} ->
      ReopenCommentThreadEventChangeDTO' $ toReopenCommentThreadEventChangeDTO event
    DeleteCommentThreadEvent' event@DeleteCommentThreadEvent {..} ->
      DeleteCommentThreadEventChangeDTO' $ toDeleteCommentThreadEventChangeDTO event
    AddCommentEvent' event@AddCommentEvent {..} -> AddCommentEventChangeDTO' $ toAddCommentEventChangeDTO event
    EditCommentEvent' event@EditCommentEvent {..} -> EditCommentEventChangeDTO' $ toEditCommentEventChangeDTO event
    DeleteCommentEvent' event@DeleteCommentEvent {..} ->
      DeleteCommentEventChangeDTO' $ toDeleteCommentEventChangeDTO event

toSetReplyEventChangeDTO :: SetReplyEvent -> SetReplyEventChangeDTO
toSetReplyEventChangeDTO event =
  SetReplyEventChangeDTO
    { _setReplyEventChangeDTOUuid = event ^. uuid
    , _setReplyEventChangeDTOPath = event ^. path
    , _setReplyEventChangeDTOValue = event ^. value
    }

toClearReplyEventChangeDTO :: ClearReplyEvent -> ClearReplyEventChangeDTO
toClearReplyEventChangeDTO event =
  ClearReplyEventChangeDTO
    {_clearReplyEventChangeDTOUuid = event ^. uuid, _clearReplyEventChangeDTOPath = event ^. path}

toSetPhaseEventChangeDTO :: SetPhaseEvent -> SetPhaseEventChangeDTO
toSetPhaseEventChangeDTO event =
  SetPhaseEventChangeDTO
    {_setPhaseEventChangeDTOUuid = event ^. uuid, _setPhaseEventChangeDTOPhaseUuid = event ^. phaseUuid}

toSetLabelsEventChangeDTO :: SetLabelsEvent -> SetLabelsEventChangeDTO
toSetLabelsEventChangeDTO event =
  SetLabelsEventChangeDTO
    { _setLabelsEventChangeDTOUuid = event ^. uuid
    , _setLabelsEventChangeDTOPath = event ^. path
    , _setLabelsEventChangeDTOValue = event ^. value
    }

toResolveCommentThreadEventChangeDTO :: ResolveCommentThreadEvent -> ResolveCommentThreadEventChangeDTO
toResolveCommentThreadEventChangeDTO event =
  ResolveCommentThreadEventChangeDTO
    { _resolveCommentThreadEventChangeDTOUuid = event ^. uuid
    , _resolveCommentThreadEventChangeDTOPath = event ^. path
    , _resolveCommentThreadEventChangeDTOThreadUuid = event ^. threadUuid
    }

toReopenCommentThreadEventChangeDTO :: ReopenCommentThreadEvent -> ReopenCommentThreadEventChangeDTO
toReopenCommentThreadEventChangeDTO event =
  ReopenCommentThreadEventChangeDTO
    { _reopenCommentThreadEventChangeDTOUuid = event ^. uuid
    , _reopenCommentThreadEventChangeDTOPath = event ^. path
    , _reopenCommentThreadEventChangeDTOThreadUuid = event ^. threadUuid
    }

toDeleteCommentThreadEventChangeDTO :: DeleteCommentThreadEvent -> DeleteCommentThreadEventChangeDTO
toDeleteCommentThreadEventChangeDTO event =
  DeleteCommentThreadEventChangeDTO
    { _deleteCommentThreadEventChangeDTOUuid = event ^. uuid
    , _deleteCommentThreadEventChangeDTOPath = event ^. path
    , _deleteCommentThreadEventChangeDTOThreadUuid = event ^. threadUuid
    }

toAddCommentEventChangeDTO :: AddCommentEvent -> AddCommentEventChangeDTO
toAddCommentEventChangeDTO event =
  AddCommentEventChangeDTO
    { _addCommentEventChangeDTOUuid = event ^. uuid
    , _addCommentEventChangeDTOPath = event ^. path
    , _addCommentEventChangeDTOThreadUuid = event ^. threadUuid
    , _addCommentEventChangeDTOCommentUuid = event ^. commentUuid
    , _addCommentEventChangeDTOText = event ^. text
    }

toEditCommentEventChangeDTO :: EditCommentEvent -> EditCommentEventChangeDTO
toEditCommentEventChangeDTO event =
  EditCommentEventChangeDTO
    { _editCommentEventChangeDTOUuid = event ^. uuid
    , _editCommentEventChangeDTOPath = event ^. path
    , _editCommentEventChangeDTOThreadUuid = event ^. threadUuid
    , _editCommentEventChangeDTOCommentUuid = event ^. commentUuid
    , _editCommentEventChangeDTOText = event ^. text
    }

toDeleteCommentEventChangeDTO :: DeleteCommentEvent -> DeleteCommentEventChangeDTO
toDeleteCommentEventChangeDTO event =
  DeleteCommentEventChangeDTO
    { _deleteCommentEventChangeDTOUuid = event ^. uuid
    , _deleteCommentEventChangeDTOPath = event ^. path
    , _deleteCommentEventChangeDTOThreadUuid = event ^. threadUuid
    , _deleteCommentEventChangeDTOCommentUuid = event ^. commentUuid
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
fromEventDTO :: QuestionnaireEventDTO -> QuestionnaireEvent
fromEventDTO event =
  case event of
    SetReplyEventDTO' event@SetReplyEventDTO {..} -> SetReplyEvent' $ fromSetReplyEventDTO event
    ClearReplyEventDTO' event@ClearReplyEventDTO {..} -> ClearReplyEvent' $ fromClearReplyEventDTO event
    SetPhaseEventDTO' event@SetPhaseEventDTO {..} -> SetPhaseEvent' $ fromSetPhaseEventDTO event
    SetLabelsEventDTO' event@SetLabelsEventDTO {..} -> SetLabelsEvent' $ fromSetLabelsEventDTO event
    ResolveCommentThreadEventDTO' event@ResolveCommentThreadEventDTO {..} ->
      ResolveCommentThreadEvent' $ fromResolveCommentThreadEventDTO event
    ReopenCommentThreadEventDTO' event@ReopenCommentThreadEventDTO {..} ->
      ReopenCommentThreadEvent' $ fromReopenCommentThreadEventDTO event
    DeleteCommentThreadEventDTO' event@DeleteCommentThreadEventDTO {..} ->
      DeleteCommentThreadEvent' $ fromDeleteCommentThreadEventDTO event
    AddCommentEventDTO' event@AddCommentEventDTO {..} -> AddCommentEvent' $ fromAddCommentEventDTO event
    EditCommentEventDTO' event@EditCommentEventDTO {..} -> EditCommentEvent' $ fromEditCommentEventDTO event
    DeleteCommentEventDTO' event@DeleteCommentEventDTO {..} -> DeleteCommentEvent' $ fromDeleteCommentEventDTO event

fromSetReplyEventDTO :: SetReplyEventDTO -> SetReplyEvent
fromSetReplyEventDTO event =
  SetReplyEvent
    { _setReplyEventUuid = event ^. uuid
    , _setReplyEventPath = event ^. path
    , _setReplyEventValue = event ^. value
    , _setReplyEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _setReplyEventCreatedAt = event ^. createdAt
    }

fromClearReplyEventDTO :: ClearReplyEventDTO -> ClearReplyEvent
fromClearReplyEventDTO event =
  ClearReplyEvent
    { _clearReplyEventUuid = event ^. uuid
    , _clearReplyEventPath = event ^. path
    , _clearReplyEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _clearReplyEventCreatedAt = event ^. createdAt
    }

fromSetPhaseEventDTO :: SetPhaseEventDTO -> SetPhaseEvent
fromSetPhaseEventDTO event =
  SetPhaseEvent
    { _setPhaseEventUuid = event ^. uuid
    , _setPhaseEventPhaseUuid = event ^. phaseUuid
    , _setPhaseEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _setPhaseEventCreatedAt = event ^. createdAt
    }

fromSetLabelsEventDTO :: SetLabelsEventDTO -> SetLabelsEvent
fromSetLabelsEventDTO event =
  SetLabelsEvent
    { _setLabelsEventUuid = event ^. uuid
    , _setLabelsEventPath = event ^. path
    , _setLabelsEventValue = event ^. value
    , _setLabelsEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _setLabelsEventCreatedAt = event ^. createdAt
    }

fromResolveCommentThreadEventDTO :: ResolveCommentThreadEventDTO -> ResolveCommentThreadEvent
fromResolveCommentThreadEventDTO event =
  ResolveCommentThreadEvent
    { _resolveCommentThreadEventUuid = event ^. uuid
    , _resolveCommentThreadEventPath = event ^. path
    , _resolveCommentThreadEventThreadUuid = event ^. threadUuid
    , _resolveCommentThreadEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _resolveCommentThreadEventCreatedAt = event ^. createdAt
    }

fromReopenCommentThreadEventDTO :: ReopenCommentThreadEventDTO -> ReopenCommentThreadEvent
fromReopenCommentThreadEventDTO event =
  ReopenCommentThreadEvent
    { _reopenCommentThreadEventUuid = event ^. uuid
    , _reopenCommentThreadEventPath = event ^. path
    , _reopenCommentThreadEventThreadUuid = event ^. threadUuid
    , _reopenCommentThreadEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _reopenCommentThreadEventCreatedAt = event ^. createdAt
    }

fromDeleteCommentThreadEventDTO :: DeleteCommentThreadEventDTO -> DeleteCommentThreadEvent
fromDeleteCommentThreadEventDTO event =
  DeleteCommentThreadEvent
    { _deleteCommentThreadEventUuid = event ^. uuid
    , _deleteCommentThreadEventPath = event ^. path
    , _deleteCommentThreadEventThreadUuid = event ^. threadUuid
    , _deleteCommentThreadEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _deleteCommentThreadEventCreatedAt = event ^. createdAt
    }

fromAddCommentEventDTO :: AddCommentEventDTO -> AddCommentEvent
fromAddCommentEventDTO event =
  AddCommentEvent
    { _addCommentEventUuid = event ^. uuid
    , _addCommentEventPath = event ^. path
    , _addCommentEventThreadUuid = event ^. threadUuid
    , _addCommentEventCommentUuid = event ^. commentUuid
    , _addCommentEventText = event ^. text
    , _addCommentEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _addCommentEventCreatedAt = event ^. createdAt
    }

fromEditCommentEventDTO :: EditCommentEventDTO -> EditCommentEvent
fromEditCommentEventDTO event =
  EditCommentEvent
    { _editCommentEventUuid = event ^. uuid
    , _editCommentEventPath = event ^. path
    , _editCommentEventThreadUuid = event ^. threadUuid
    , _editCommentEventCommentUuid = event ^. commentUuid
    , _editCommentEventText = event ^. text
    , _editCommentEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _editCommentEventCreatedAt = event ^. createdAt
    }

fromDeleteCommentEventDTO :: DeleteCommentEventDTO -> DeleteCommentEvent
fromDeleteCommentEventDTO event =
  DeleteCommentEvent
    { _deleteCommentEventUuid = event ^. uuid
    , _deleteCommentEventPath = event ^. path
    , _deleteCommentEventThreadUuid = event ^. threadUuid
    , _deleteCommentEventCommentUuid = event ^. commentUuid
    , _deleteCommentEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _deleteCommentEventCreatedAt = event ^. createdAt
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
    ResolveCommentThreadEventChangeDTO' event@ResolveCommentThreadEventChangeDTO {..} ->
      ResolveCommentThreadEvent' $ fromResolveCommentThreadEventChangeDTO event createdBy now
    ReopenCommentThreadEventChangeDTO' event@ReopenCommentThreadEventChangeDTO {..} ->
      ReopenCommentThreadEvent' $ fromReopenCommentThreadEventChangeDTO event createdBy now
    DeleteCommentThreadEventChangeDTO' event@DeleteCommentThreadEventChangeDTO {..} ->
      DeleteCommentThreadEvent' $ fromDeleteCommentThreadEventChangeDTO event createdBy now
    AddCommentEventChangeDTO' event@AddCommentEventChangeDTO {..} ->
      AddCommentEvent' $ fromAddCommentEventChangeDTO event createdBy now
    EditCommentEventChangeDTO' event@EditCommentEventChangeDTO {..} ->
      EditCommentEvent' $ fromEditCommentEventChangeDTO event createdBy now
    DeleteCommentEventChangeDTO' event@DeleteCommentEventChangeDTO {..} ->
      DeleteCommentEvent' $ fromDeleteCommentEventChangeDTO event createdBy now

fromSetReplyEventChangeDTO :: SetReplyEventChangeDTO -> Maybe U.UUID -> UTCTime -> SetReplyEvent
fromSetReplyEventChangeDTO event createdBy now =
  SetReplyEvent
    { _setReplyEventUuid = event ^. uuid
    , _setReplyEventPath = event ^. path
    , _setReplyEventValue = event ^. value
    , _setReplyEventCreatedBy = createdBy
    , _setReplyEventCreatedAt = now
    }

fromClearReplyEventChangeDTO :: ClearReplyEventChangeDTO -> Maybe U.UUID -> UTCTime -> ClearReplyEvent
fromClearReplyEventChangeDTO event createdBy now =
  ClearReplyEvent
    { _clearReplyEventUuid = event ^. uuid
    , _clearReplyEventPath = event ^. path
    , _clearReplyEventCreatedBy = createdBy
    , _clearReplyEventCreatedAt = now
    }

fromSetPhaseEventChangeDTO :: SetPhaseEventChangeDTO -> Maybe U.UUID -> UTCTime -> SetPhaseEvent
fromSetPhaseEventChangeDTO event createdBy now =
  SetPhaseEvent
    { _setPhaseEventUuid = event ^. uuid
    , _setPhaseEventPhaseUuid = event ^. phaseUuid
    , _setPhaseEventCreatedBy = createdBy
    , _setPhaseEventCreatedAt = now
    }

fromSetLabelsEventChangeDTO :: SetLabelsEventChangeDTO -> Maybe U.UUID -> UTCTime -> SetLabelsEvent
fromSetLabelsEventChangeDTO event createdBy now =
  SetLabelsEvent
    { _setLabelsEventUuid = event ^. uuid
    , _setLabelsEventPath = event ^. path
    , _setLabelsEventValue = event ^. value
    , _setLabelsEventCreatedBy = createdBy
    , _setLabelsEventCreatedAt = now
    }

fromResolveCommentThreadEventChangeDTO ::
     ResolveCommentThreadEventChangeDTO -> Maybe U.UUID -> UTCTime -> ResolveCommentThreadEvent
fromResolveCommentThreadEventChangeDTO event createdBy now =
  ResolveCommentThreadEvent
    { _resolveCommentThreadEventUuid = event ^. uuid
    , _resolveCommentThreadEventPath = event ^. path
    , _resolveCommentThreadEventThreadUuid = event ^. threadUuid
    , _resolveCommentThreadEventCreatedBy = createdBy
    , _resolveCommentThreadEventCreatedAt = now
    }

fromReopenCommentThreadEventChangeDTO ::
     ReopenCommentThreadEventChangeDTO -> Maybe U.UUID -> UTCTime -> ReopenCommentThreadEvent
fromReopenCommentThreadEventChangeDTO event createdBy now =
  ReopenCommentThreadEvent
    { _reopenCommentThreadEventUuid = event ^. uuid
    , _reopenCommentThreadEventPath = event ^. path
    , _reopenCommentThreadEventThreadUuid = event ^. threadUuid
    , _reopenCommentThreadEventCreatedBy = createdBy
    , _reopenCommentThreadEventCreatedAt = now
    }

fromDeleteCommentThreadEventChangeDTO ::
     DeleteCommentThreadEventChangeDTO -> Maybe U.UUID -> UTCTime -> DeleteCommentThreadEvent
fromDeleteCommentThreadEventChangeDTO event createdBy now =
  DeleteCommentThreadEvent
    { _deleteCommentThreadEventUuid = event ^. uuid
    , _deleteCommentThreadEventPath = event ^. path
    , _deleteCommentThreadEventThreadUuid = event ^. threadUuid
    , _deleteCommentThreadEventCreatedBy = createdBy
    , _deleteCommentThreadEventCreatedAt = now
    }

fromAddCommentEventChangeDTO :: AddCommentEventChangeDTO -> Maybe U.UUID -> UTCTime -> AddCommentEvent
fromAddCommentEventChangeDTO event createdBy now =
  AddCommentEvent
    { _addCommentEventUuid = event ^. uuid
    , _addCommentEventPath = event ^. path
    , _addCommentEventThreadUuid = event ^. threadUuid
    , _addCommentEventCommentUuid = event ^. commentUuid
    , _addCommentEventText = event ^. text
    , _addCommentEventCreatedBy = createdBy
    , _addCommentEventCreatedAt = now
    }

fromEditCommentEventChangeDTO :: EditCommentEventChangeDTO -> Maybe U.UUID -> UTCTime -> EditCommentEvent
fromEditCommentEventChangeDTO event createdBy now =
  EditCommentEvent
    { _editCommentEventUuid = event ^. uuid
    , _editCommentEventPath = event ^. path
    , _editCommentEventThreadUuid = event ^. threadUuid
    , _editCommentEventCommentUuid = event ^. commentUuid
    , _editCommentEventText = event ^. text
    , _editCommentEventCreatedBy = createdBy
    , _editCommentEventCreatedAt = now
    }

fromDeleteCommentEventChangeDTO :: DeleteCommentEventChangeDTO -> Maybe U.UUID -> UTCTime -> DeleteCommentEvent
fromDeleteCommentEventChangeDTO event createdBy now =
  DeleteCommentEvent
    { _deleteCommentEventUuid = event ^. uuid
    , _deleteCommentEventPath = event ^. path
    , _deleteCommentEventThreadUuid = event ^. threadUuid
    , _deleteCommentEventCommentUuid = event ^. commentUuid
    , _deleteCommentEventCreatedBy = createdBy
    , _deleteCommentEventCreatedAt = now
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toReply :: SetReplyEvent -> Maybe User -> Reply
toReply event mUser =
  Reply
    { _replyValue = event ^. value
    , _replyCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , _replyCreatedAt = event ^. createdAt
    }

toCommentThread :: AddCommentEvent -> Maybe User -> QuestionnaireCommentThread
toCommentThread event mUser =
  QuestionnaireCommentThread
    { _questionnaireCommentThreadUuid = event ^. threadUuid
    , _questionnaireCommentThreadResolved = False
    , _questionnaireCommentThreadComments = [toComment event mUser]
    , _questionnaireCommentThreadCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , _questionnaireCommentThreadCreatedAt = event ^. createdAt
    , _questionnaireCommentThreadUpdatedAt = event ^. createdAt
    }

toComment :: AddCommentEvent -> Maybe User -> QuestionnaireComment
toComment event mUser =
  QuestionnaireComment
    { _questionnaireCommentUuid = event ^. commentUuid
    , _questionnaireCommentText = event ^. text
    , _questionnaireCommentCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , _questionnaireCommentCreatedAt = event ^. createdAt
    , _questionnaireCommentUpdatedAt = event ^. createdAt
    }
