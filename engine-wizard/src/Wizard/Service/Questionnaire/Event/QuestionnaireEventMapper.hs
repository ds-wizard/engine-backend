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
    SetLevelEvent' event@SetLevelEvent {..} -> SetLevelEventDTO' $ toSetLevelEventDTO event mCreatedBy
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

toSetLevelEventDTO :: SetLevelEvent -> Maybe User -> SetLevelEventDTO
toSetLevelEventDTO event user =
  SetLevelEventDTO
    { _setLevelEventDTOUuid = event ^. uuid
    , _setLevelEventDTOLevel = event ^. level
    , _setLevelEventDTOCreatedBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) user
    , _setLevelEventDTOCreatedAt = event ^. createdAt
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
    SetLevelEventChangeDTO' event@SetLevelEventChangeDTO {..} ->
      SetLevelEventDTO' $ toSetLevelEventDTO' event mCreatedBy now
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

toSetLevelEventDTO' :: SetLevelEventChangeDTO -> Maybe UserSuggestionDTO -> UTCTime -> SetLevelEventDTO
toSetLevelEventDTO' event mCreatedBy now =
  SetLevelEventDTO
    { _setLevelEventDTOUuid = event ^. uuid
    , _setLevelEventDTOLevel = event ^. level
    , _setLevelEventDTOCreatedBy = mCreatedBy
    , _setLevelEventDTOCreatedAt = now
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
    SetLevelEvent' event@SetLevelEvent {..} -> SetLevelEventChangeDTO' $ toSetLevelEventChangeDTO event
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

toSetLevelEventChangeDTO :: SetLevelEvent -> SetLevelEventChangeDTO
toSetLevelEventChangeDTO event =
  SetLevelEventChangeDTO {_setLevelEventChangeDTOUuid = event ^. uuid, _setLevelEventChangeDTOLevel = event ^. level}

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
    SetLevelEventDTO' event@SetLevelEventDTO {..} -> SetLevelEvent' $ fromSetLevelEventDTO event
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

fromSetLevelEventDTO :: SetLevelEventDTO -> SetLevelEvent
fromSetLevelEventDTO event =
  SetLevelEvent
    { _setLevelEventUuid = event ^. uuid
    , _setLevelEventLevel = event ^. level
    , _setLevelEventCreatedBy = event ^. createdBy ^? _Just . uuid
    , _setLevelEventCreatedAt = event ^. createdAt
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
    SetLevelEventChangeDTO' event@SetLevelEventChangeDTO {..} ->
      SetLevelEvent' $ fromSetLevelEventChangeDTO event createdBy now
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

fromSetLevelEventChangeDTO :: SetLevelEventChangeDTO -> Maybe U.UUID -> UTCTime -> SetLevelEvent
fromSetLevelEventChangeDTO event createdBy now =
  SetLevelEvent
    { _setLevelEventUuid = event ^. uuid
    , _setLevelEventLevel = event ^. level
    , _setLevelEventCreatedBy = createdBy
    , _setLevelEventCreatedAt = now
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
