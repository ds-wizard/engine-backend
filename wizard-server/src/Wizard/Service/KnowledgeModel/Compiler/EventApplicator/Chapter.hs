module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Chapter where

import qualified Data.List as L
import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddChapterEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {chapterUuids = km.chapterUuids ++ [event.entityUuid]}
      addEntity = putInChaptersM event.entityUuid (createEntity event content)

instance ApplyEvent EditChapterEvent where
  apply = applyEditEvent getChaptersM setChaptersM

instance ApplyEvent DeleteChapterEvent where
  apply event content = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference km = km {chapterUuids = L.delete event.entityUuid km.chapterUuids}
      deleteEntity km = deleteChapter km event.entityUuid
