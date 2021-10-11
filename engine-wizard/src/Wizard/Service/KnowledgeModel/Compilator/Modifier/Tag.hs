module Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddTagEvent Tag where
  createEntity e =
    Tag
      { _tagUuid = e ^. entityUuid
      , _tagName = e ^. name
      , _tagDescription = e ^. description
      , _tagColor = e ^. color
      , _tagAnnotations = e ^. annotations
      }

instance EditEntity EditTagEvent Tag where
  editEntity e = applyAnnotations . applyColor . applyDescription . applyName
    where
      applyName tag = applyValue (e ^. name) tag name
      applyDescription tag = applyValue (e ^. description) tag description
      applyColor tag = applyValue (e ^. color) tag color
      applyAnnotations tag = applyValue (e ^. annotations) tag annotations
