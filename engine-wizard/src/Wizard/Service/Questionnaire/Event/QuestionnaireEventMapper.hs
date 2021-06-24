module Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper where

import Control.Lens ((^.), (^?), _Just)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
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

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toEventChangeDTO :: QuestionnaireEvent -> QuestionnaireEventChangeDTO
toEventChangeDTO event =
  case event of
    SetReplyEvent' event@SetReplyEvent {..} -> SetReplyEventChangeDTO' $ toSetReplyEventChangeDTO event
    ClearReplyEvent' event@ClearReplyEvent {..} -> ClearReplyEventChangeDTO' $ toClearReplyEventChangeDTO event
    SetPhaseEvent' event@SetPhaseEvent {..} -> SetPhaseEventChangeDTO' $ toSetPhaseEventChangeDTO event
    SetLabelsEvent' event@SetLabelsEvent {..} -> SetLabelsEventChangeDTO' $ toSetLabelsEventChangeDTO event

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

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
fromEventDTO :: QuestionnaireEventDTO -> QuestionnaireEvent
fromEventDTO event =
  case event of
    SetReplyEventDTO' event@SetReplyEventDTO {..} -> SetReplyEvent' $ fromSetReplyEventDTO event
    ClearReplyEventDTO' event@ClearReplyEventDTO {..} -> ClearReplyEvent' $ fromClearReplyEventDTO event
    SetPhaseEventDTO' event@SetPhaseEventDTO {..} -> SetPhaseEvent' $ fromSetPhaseEventDTO event
    SetLabelsEventDTO' event@SetLabelsEventDTO {..} -> SetLabelsEvent' $ fromSetLabelsEventDTO event

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

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toReply :: SetReplyEvent -> Maybe User -> Reply
toReply event mUser =
  Reply
    { _replyValue = event ^. value
    , _replyCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mUser
    , _replyCreatedAt = event ^. createdAt
    }
