module Service.KnowledgeModel.Compilator.EventApplicator.KnowledgeModel where

import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Service.KnowledgeModel.Compilator.Modifier.Modifier
import Service.KnowledgeModel.Compilator.Modifier.Question ()
import Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddKnowledgeModelEvent where
  apply e _ = Right . createEntity $ e

instance ApplyEvent EditKnowledgeModelEvent where
  apply e km = Right . editEntity e $ km
