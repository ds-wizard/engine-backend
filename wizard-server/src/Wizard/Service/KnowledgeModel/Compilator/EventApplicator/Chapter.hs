module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Chapter where

import qualified Data.List as L
import Prelude hiding (lookup)

import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
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
