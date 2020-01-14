module Wizard.Model.Event.EventLenses where

import Control.Lens
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.EventField
import Shared.Model.Event.Question.QuestionEvent

eUuid' :: Functor f => (U.UUID -> f U.UUID) -> EditQuestionEvent -> f EditQuestionEvent
eUuid' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: EditQuestionEvent -> U.UUID
    get (EditListQuestionEvent' e) = e ^. uuid
    get (EditOptionsQuestionEvent' e) = e ^. uuid
    get (EditValueQuestionEvent' e) = e ^. uuid
    get (EditIntegrationQuestionEvent' e) = e ^. uuid
    set :: EditQuestionEvent -> U.UUID -> EditQuestionEvent
    set (EditListQuestionEvent' e) newValue = EditListQuestionEvent' $ e & uuid .~ newValue
    set (EditOptionsQuestionEvent' e) newValue = EditOptionsQuestionEvent' $ e & uuid .~ newValue
    set (EditValueQuestionEvent' e) newValue = EditValueQuestionEvent' $ e & uuid .~ newValue
    set (EditIntegrationQuestionEvent' e) newValue = EditIntegrationQuestionEvent' $ e & uuid .~ newValue

eExpertUuids' ::
     Functor f => (EventField [U.UUID] -> f (EventField [U.UUID])) -> EditQuestionEvent -> f EditQuestionEvent
eExpertUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: EditQuestionEvent -> EventField [U.UUID]
    get (EditListQuestionEvent' e) = e ^. expertUuids
    get (EditOptionsQuestionEvent' e) = e ^. expertUuids
    get (EditValueQuestionEvent' e) = e ^. expertUuids
    get (EditIntegrationQuestionEvent' e) = e ^. expertUuids
    set :: EditQuestionEvent -> EventField [U.UUID] -> EditQuestionEvent
    set (EditListQuestionEvent' e) newValue = EditListQuestionEvent' $ e & expertUuids .~ newValue
    set (EditOptionsQuestionEvent' e) newValue = EditOptionsQuestionEvent' $ e & expertUuids .~ newValue
    set (EditValueQuestionEvent' e) newValue = EditValueQuestionEvent' $ e & expertUuids .~ newValue
    set (EditIntegrationQuestionEvent' e) newValue = EditIntegrationQuestionEvent' $ e & expertUuids .~ newValue

eReferenceUuids' ::
     Functor f => (EventField [U.UUID] -> f (EventField [U.UUID])) -> EditQuestionEvent -> f EditQuestionEvent
eReferenceUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: EditQuestionEvent -> EventField [U.UUID]
    get (EditListQuestionEvent' e) = e ^. referenceUuids
    get (EditOptionsQuestionEvent' e) = e ^. referenceUuids
    get (EditValueQuestionEvent' e) = e ^. referenceUuids
    get (EditIntegrationQuestionEvent' e) = e ^. referenceUuids
    set :: EditQuestionEvent -> EventField [U.UUID] -> EditQuestionEvent
    set (EditListQuestionEvent' e) newValue = EditListQuestionEvent' $ e & referenceUuids .~ newValue
    set (EditOptionsQuestionEvent' e) newValue = EditOptionsQuestionEvent' $ e & referenceUuids .~ newValue
    set (EditValueQuestionEvent' e) newValue = EditValueQuestionEvent' $ e & referenceUuids .~ newValue
    set (EditIntegrationQuestionEvent' e) newValue = EditIntegrationQuestionEvent' $ e & referenceUuids .~ newValue
