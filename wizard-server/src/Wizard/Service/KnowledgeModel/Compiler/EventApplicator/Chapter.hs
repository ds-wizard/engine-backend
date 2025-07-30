module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Chapter where

import qualified Data.List as L
import Prelude hiding (lookup)

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
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance ApplyEvent AddChapterEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {chapterUuids = km.chapterUuids ++ [getEntityUuid event]}
      addEntity = putInChaptersM (getEntityUuid event) (createEntity event)

instance ApplyEvent EditChapterEvent where
  apply = applyEditEvent getChaptersM setChaptersM

instance ApplyEvent DeleteChapterEvent where
  apply event = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference km = km {chapterUuids = L.delete (getEntityUuid event) km.chapterUuids}
      deleteEntity km = deleteChapter km (getEntityUuid event)
