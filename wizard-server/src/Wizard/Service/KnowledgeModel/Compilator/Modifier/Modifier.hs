module Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier where

import WizardLib.KnowledgeModel.Model.Event.EventField

applyValue _ (ChangedValue value) = value
applyValue defaultValue NothingChanged = defaultValue

class CreateEntity event entity where
  createEntity :: event -> entity

class EditEntity event entity where
  editEntity :: event -> entity -> entity
