module Wizard.Service.KnowledgeModel.Compilator.Modifier.Choice where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddChoiceEvent Choice where
  createEntity e = Choice {_choiceUuid = e ^. entityUuid, _choiceLabel = e ^. label}

instance EditEntity EditChoiceEvent Choice where
  editEntity e = applyLabel
    where
      applyLabel ans = applyValue (e ^. label) ans label
