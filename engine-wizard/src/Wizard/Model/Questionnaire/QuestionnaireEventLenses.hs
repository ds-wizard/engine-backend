module Wizard.Model.Questionnaire.QuestionnaireEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Wizard.Model.Questionnaire.QuestionnaireEvent

instance HasUuid' QuestionnaireEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: QuestionnaireEvent -> U.UUID
      get (SetReplyEvent' entity) = entity ^. uuid
      get (ClearReplyEvent' entity) = entity ^. uuid
      get (SetPhaseEvent' entity) = entity ^. uuid
      get (SetLabelsEvent' entity) = entity ^. uuid
      set :: QuestionnaireEvent -> U.UUID -> QuestionnaireEvent
      set (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity & uuid .~ newValue
      set (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity & uuid .~ newValue
      set (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity & uuid .~ newValue
      set (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity & uuid .~ newValue

instance HasCreatedAt' QuestionnaireEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: QuestionnaireEvent -> UTCTime
      get (SetReplyEvent' entity) = entity ^. createdAt
      get (ClearReplyEvent' entity) = entity ^. createdAt
      get (SetPhaseEvent' entity) = entity ^. createdAt
      get (SetLabelsEvent' entity) = entity ^. createdAt
      set :: QuestionnaireEvent -> UTCTime -> QuestionnaireEvent
      set (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity & createdAt .~ newValue
      set (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity & createdAt .~ newValue
      set (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity & createdAt .~ newValue
      set (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity & createdAt .~ newValue

instance HasCreatedBy' QuestionnaireEvent where
  createdBy' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: QuestionnaireEvent -> Maybe U.UUID
      get (SetReplyEvent' entity) = entity ^. createdBy
      get (ClearReplyEvent' entity) = entity ^. createdBy
      get (SetPhaseEvent' entity) = entity ^. createdBy
      get (SetLabelsEvent' entity) = entity ^. createdBy
      set :: QuestionnaireEvent -> Maybe U.UUID -> QuestionnaireEvent
      set (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity & createdBy .~ newValue
      set (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity & createdBy .~ newValue
      set (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity & createdBy .~ newValue
      set (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity & createdBy .~ newValue
