module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Tag where

import Control.Lens
import qualified Data.Map as M
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.EventLenses
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
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
      addEntityReference km = km & ap tagUuids .~ (event ^. entityUuid')
      addEntity km = km & tagsM . at (event ^. entityUuid') ?~ createEntity event

instance ApplyEvent EditTagEvent where
  apply = applyEditEvent (entities . tags) "Tag"

instance ApplyEvent DeleteTagEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km & del tagUuids .~ (event ^. entityUuid')
      deleteEntity km = km & tagsM .~ M.delete (event ^. entityUuid') (km ^. tagsM)
      deleteEntityChildrenReference km =
        km & entities . questions .~ M.map (deleteTagReference event) (km ^. entities . questions)
