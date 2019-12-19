module Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier where

import Control.Lens ((&), (.~))

import Shared.Model.Event.EventField

applyValue (ChangedValue val) ch setter = ch & setter .~ val
applyValue NothingChanged ch setter = ch

class CreateEntity event entity where
  createEntity :: event -> entity

class EditEntity event entity where
  editEntity :: event -> entity -> entity
