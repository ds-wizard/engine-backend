module Wizard.Service.Owl.Diff.EventFactory.EventFactory where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.UUID as U

import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

class EventFactory entity where
  createAddEvent :: MonadIO m => U.UUID -> entity -> m Event
  createEditEvent :: MonadIO m => (KnowledgeModel, KnowledgeModel) -> U.UUID -> entity -> entity -> m (Maybe Event)
  createDeleteEvent :: MonadIO m => U.UUID -> entity -> m Event
