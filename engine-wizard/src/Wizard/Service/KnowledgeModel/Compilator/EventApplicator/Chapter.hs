module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Chapter where

import Control.Lens
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.EventLenses
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
import Wizard.Util.Lens

instance ApplyEvent AddChapterEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km & ap chapterUuids .~ (event ^. entityUuid')
      addEntity km = km & chaptersM . at (event ^. entityUuid') ?~ createEntity event

instance ApplyEvent EditChapterEvent where
  apply = applyEditEvent (entities . chapters) "Chapter"

instance ApplyEvent DeleteChapterEvent where
  apply event = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference km = km & del chapterUuids .~ (event ^. entityUuid')
      deleteEntity km = deleteChapter km (event ^. entityUuid')
