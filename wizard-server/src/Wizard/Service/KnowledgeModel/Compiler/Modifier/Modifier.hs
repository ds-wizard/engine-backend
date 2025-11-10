module Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

applyValue _ (ChangedValue value) = value
applyValue defaultValue NothingChanged = defaultValue

class CreateEntity content entity where
  createEntity :: KnowledgeModelEvent -> content -> entity

class EditEntity content entity where
  editEntity :: KnowledgeModelEvent -> content -> entity -> entity
