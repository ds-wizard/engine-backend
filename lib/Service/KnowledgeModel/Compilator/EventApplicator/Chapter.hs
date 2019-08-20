module Service.KnowledgeModel.Compilator.EventApplicator.Chapter where

import Control.Lens
import Prelude hiding (lookup)

import LensesConfig
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventAccessors
import Model.KnowledgeModel.KnowledgeModelLenses
import Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Service.KnowledgeModel.Compilator.Modifier.Delete
import Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Service.KnowledgeModel.Compilator.Modifier.Modifier
import Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Service.KnowledgeModel.Compilator.Modifier.Tag ()
import Util.Lens

instance ApplyEvent AddChapterEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km & (ap chapterUuids) .~ (getEventNodeUuid event)
      addEntity km = km & (chaptersM . at (getEventNodeUuid event)) ?~ (createEntity event)

instance ApplyEvent EditChapterEvent where
  apply = applyEditEvent (entities . chapters) "Chapter"

instance ApplyEvent DeleteChapterEvent where
  apply event = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference km = km & del chapterUuids .~ (getEventNodeUuid event)
      deleteEntity km = deleteChapter km (getEventNodeUuid event)
