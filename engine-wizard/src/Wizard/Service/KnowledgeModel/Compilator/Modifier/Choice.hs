module Wizard.Service.KnowledgeModel.Compilator.Modifier.Choice where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddChoiceEvent Choice where
  createEntity e =
    Choice {_choiceUuid = e ^. entityUuid, _choiceLabel = e ^. label, _choiceAnnotations = e ^. annotations}

instance EditEntity EditChoiceEvent Choice where
  editEntity e = applyAnnotations . applyLabel
    where
      applyLabel ch = applyValue (e ^. label) ch label
      applyAnnotations ch = applyValue (e ^. annotations) ch annotations
