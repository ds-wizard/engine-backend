module Service.KnowledgeModel.Compilator.Modifier.Tag where

import Control.Lens ((^.))

import LensesConfig
import Model.Event.Tag.TagEvent
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddTagEvent Tag where
  createEntity e =
    Tag {_tagUuid = e ^. entityUuid, _tagName = e ^. name, _tagDescription = e ^. description, _tagColor = e ^. color}

instance EditEntity EditTagEvent Tag where
  editEntity e = applyColor . applyDescription . applyName
    where
      applyName tag = applyValue (e ^. name) tag name
      applyDescription tag = applyValue (e ^. description) tag description
      applyColor tag = applyValue (e ^. color) tag color
