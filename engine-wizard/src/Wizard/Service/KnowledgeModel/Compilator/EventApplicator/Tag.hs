module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Tag where

import Control.Lens
import qualified Data.Map as M
import Prelude hiding (lookup)

import Shared.Model.Event.EventAccessors
import Shared.Model.Event.Tag.TagEvent
import Wizard.LensesConfig
import Wizard.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
import Wizard.Util.Lens

instance ApplyEvent AddTagEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km & (ap tagUuids) .~ (getEventNodeUuid event)
      addEntity km = km & (tagsM . at (getEventNodeUuid event)) ?~ (createEntity event)

instance ApplyEvent EditTagEvent where
  apply = applyEditEvent (entities . tags) "Tag"

instance ApplyEvent DeleteTagEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km & del tagUuids .~ (getEventNodeUuid event)
      deleteEntity km = km & tagsM .~ (M.delete (getEventNodeUuid event) (km ^. tagsM))
      deleteEntityChildrenReference km =
        km & (entities . questions) .~ (M.map (deleteTagReference event) (km ^. entities . questions))
