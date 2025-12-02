module Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference where

import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddReferenceEvent Reference where
  createEntity event (AddResourcePageReferenceEvent' content) =
    ResourcePageReference' $
      ResourcePageReference
        { uuid = event.entityUuid
        , resourcePageUuid = content.resourcePageUuid
        , annotations = content.annotations
        }
  createEntity event (AddURLReferenceEvent' content) =
    URLReference' $
      URLReference
        { uuid = event.entityUuid
        , url = content.url
        , aLabel = content.aLabel
        , annotations = content.annotations
        }
  createEntity event (AddCrossReferenceEvent' content) =
    CrossReference' $
      CrossReference
        { uuid = event.entityUuid
        , targetUuid = content.targetUuid
        , description = content.description
        , annotations = content.annotations
        }

instance EditEntity EditReferenceEvent Reference where
  editEntity event content' ref =
    case content' of
      (EditResourcePageReferenceEvent' content) -> applyToResourcePageReference content . convertToResourcePageReference $ ref
      (EditURLReferenceEvent' content) -> applyToURLReference content . convertToURLReference $ ref
      (EditCrossReferenceEvent' content) -> applyToCrossReference content . convertToCrossReference $ ref
    where
      applyToResourcePageReference content resourcePageReference =
        ResourcePageReference' $
          resourcePageReference
            { resourcePageUuid = applyValue resourcePageReference.resourcePageUuid content.resourcePageUuid
            , annotations = applyValue resourcePageReference.annotations content.annotations
            }
      applyToURLReference content urlReference =
        URLReference' $
          urlReference
            { url = applyValue urlReference.url content.url
            , aLabel = applyValue urlReference.aLabel content.aLabel
            , annotations = applyValue urlReference.annotations content.annotations
            }
      applyToCrossReference content crossReference =
        CrossReference' $
          crossReference
            { targetUuid = applyValue crossReference.targetUuid content.targetUuid
            , description = applyValue crossReference.description content.description
            , annotations = applyValue crossReference.annotations content.annotations
            }

convertToResourcePageReference :: Reference -> ResourcePageReference
convertToResourcePageReference (ResourcePageReference' ref) = ref
convertToResourcePageReference ref' =
  case ref' of
    (URLReference' ref) -> createQuestion ref
    (CrossReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      ResourcePageReference
        { uuid = ref.uuid
        , resourcePageUuid = Nothing
        , annotations = ref.annotations
        }

convertToURLReference :: Reference -> URLReference
convertToURLReference (URLReference' ref) = ref
convertToURLReference ref' =
  case ref' of
    (ResourcePageReference' ref) -> createQuestion ref
    (CrossReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      URLReference
        { uuid = ref.uuid
        , url = ""
        , aLabel = ""
        , annotations = ref.annotations
        }

convertToCrossReference :: Reference -> CrossReference
convertToCrossReference (CrossReference' ref) = ref
convertToCrossReference ref' =
  case ref' of
    (ResourcePageReference' ref) -> createQuestion ref
    (URLReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      CrossReference
        { uuid = ref.uuid
        , targetUuid = U.nil
        , description = ""
        , annotations = ref.annotations
        }
