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
      get (AddCommentEvent' entity) = entity ^. uuid
      get (EditCommentEvent' entity) = entity ^. uuid
      get (DeleteCommentThreadEvent' entity) = entity ^. uuid
      get (ResolveCommentThreadEvent' entity) = entity ^. uuid
      get (ReopenCommentThreadEvent' entity) = entity ^. uuid
      get (DeleteCommentEvent' entity) = entity ^. uuid
      set :: QuestionnaireEvent -> U.UUID -> QuestionnaireEvent
      set (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity & uuid .~ newValue
      set (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity & uuid .~ newValue
      set (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity & uuid .~ newValue
      set (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity & uuid .~ newValue
      set (AddCommentEvent' entity) newValue = AddCommentEvent' $ entity & uuid .~ newValue
      set (EditCommentEvent' entity) newValue = EditCommentEvent' $ entity & uuid .~ newValue
      set (DeleteCommentThreadEvent' entity) newValue = DeleteCommentThreadEvent' $ entity & uuid .~ newValue
      set (ResolveCommentThreadEvent' entity) newValue = ResolveCommentThreadEvent' $ entity & uuid .~ newValue
      set (ReopenCommentThreadEvent' entity) newValue = ReopenCommentThreadEvent' $ entity & uuid .~ newValue
      set (DeleteCommentEvent' entity) newValue = DeleteCommentEvent' $ entity & uuid .~ newValue

instance HasCreatedAt' QuestionnaireEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: QuestionnaireEvent -> UTCTime
      get (SetReplyEvent' entity) = entity ^. createdAt
      get (ClearReplyEvent' entity) = entity ^. createdAt
      get (SetPhaseEvent' entity) = entity ^. createdAt
      get (SetLabelsEvent' entity) = entity ^. createdAt
      get (AddCommentEvent' entity) = entity ^. createdAt
      get (EditCommentEvent' entity) = entity ^. createdAt
      get (DeleteCommentThreadEvent' entity) = entity ^. createdAt
      get (ResolveCommentThreadEvent' entity) = entity ^. createdAt
      get (ReopenCommentThreadEvent' entity) = entity ^. createdAt
      get (DeleteCommentEvent' entity) = entity ^. createdAt
      set :: QuestionnaireEvent -> UTCTime -> QuestionnaireEvent
      set (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity & createdAt .~ newValue
      set (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity & createdAt .~ newValue
      set (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity & createdAt .~ newValue
      set (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity & createdAt .~ newValue
      set (AddCommentEvent' entity) newValue = AddCommentEvent' $ entity & createdAt .~ newValue
      set (EditCommentEvent' entity) newValue = EditCommentEvent' $ entity & createdAt .~ newValue
      set (DeleteCommentThreadEvent' entity) newValue = DeleteCommentThreadEvent' $ entity & createdAt .~ newValue
      set (ResolveCommentThreadEvent' entity) newValue = ResolveCommentThreadEvent' $ entity & createdAt .~ newValue
      set (ReopenCommentThreadEvent' entity) newValue = ReopenCommentThreadEvent' $ entity & createdAt .~ newValue
      set (DeleteCommentEvent' entity) newValue = DeleteCommentEvent' $ entity & createdAt .~ newValue

instance HasCreatedBy' QuestionnaireEvent where
  createdBy' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: QuestionnaireEvent -> Maybe U.UUID
      get (SetReplyEvent' entity) = entity ^. createdBy
      get (ClearReplyEvent' entity) = entity ^. createdBy
      get (SetPhaseEvent' entity) = entity ^. createdBy
      get (SetLabelsEvent' entity) = entity ^. createdBy
      get (AddCommentEvent' entity) = entity ^. createdBy
      get (EditCommentEvent' entity) = entity ^. createdBy
      get (DeleteCommentThreadEvent' entity) = entity ^. createdBy
      get (ResolveCommentThreadEvent' entity) = entity ^. createdBy
      get (ReopenCommentThreadEvent' entity) = entity ^. createdBy
      get (DeleteCommentEvent' entity) = entity ^. createdBy
      set :: QuestionnaireEvent -> Maybe U.UUID -> QuestionnaireEvent
      set (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity & createdBy .~ newValue
      set (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity & createdBy .~ newValue
      set (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity & createdBy .~ newValue
      set (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity & createdBy .~ newValue
      set (AddCommentEvent' entity) newValue = AddCommentEvent' $ entity & createdBy .~ newValue
      set (EditCommentEvent' entity) newValue = EditCommentEvent' $ entity & createdBy .~ newValue
      set (DeleteCommentThreadEvent' entity) newValue = DeleteCommentThreadEvent' $ entity & createdBy .~ newValue
      set (ResolveCommentThreadEvent' entity) newValue = ResolveCommentThreadEvent' $ entity & createdBy .~ newValue
      set (ReopenCommentThreadEvent' entity) newValue = ReopenCommentThreadEvent' $ entity & createdBy .~ newValue
      set (DeleteCommentEvent' entity) newValue = DeleteCommentEvent' $ entity & createdBy .~ newValue
